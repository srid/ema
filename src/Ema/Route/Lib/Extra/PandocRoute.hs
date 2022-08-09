{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Lib.Extra.PandocRoute (
  -- * Route
  PandocRoute (..),
  mkPandocRoute,

  -- * Model and Arg
  Model (..),
  Arg (..),

  -- * Rendering
  PandocHtml (..),
  PandocError (..),
) where

import Control.Exception (throw, throwIO)
import Control.Monad.Logger (
  MonadLogger,
  MonadLoggerIO,
  logDebugNS,
 )
import Data.Default (Default (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ema
import Ema.CLI qualified
import Ema.Route.Prism (htmlSuffixPrism)
import GHC.TypeLits (Symbol)
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Optics.Core (prism', (%))
import System.FilePath (splitExtension, splitPath, (</>))
import System.UnionMount qualified as UnionMount
import Text.Pandoc (Pandoc, PandocMonad, ReaderOptions, runIO)
import Text.Pandoc qualified as Pandoc
import UnliftIO (MonadUnliftIO)

-- TODO: Move Ext stuff to separate module

class ToExts (syms :: [Symbol]) where
  toExts :: Proxy syms -> [String]

instance ToExts '[] where
  toExts _ = []

instance (ToExts syms, IsExt ext) => ToExts (ext ': syms) where
  toExts _ = ext : toExts @syms (Proxy :: Proxy syms)
    where
      ext = extString (Proxy :: Proxy ext)

class IsExt (ext :: k) where
  extString :: Proxy ext -> String

-- FIXME: this complects with PandocRoute

class IsPandocExt (ext :: k) where
  readExtFile :: forall exts m. (PandocMonad m, ToExts exts, MonadIO m) => Proxy ext -> FilePath -> ReaderOptions -> m (Maybe (PandocRoute exts, Pandoc))

instance IsExt ".md" where
  extString _ = ".md"

instance IsPandocExt ".md" where
  readExtFile _ fp opts = runMaybeT $ do
    (ext, r :: PandocRoute exts) <- hoistMaybe (mkPandocRoute fp)
    -- TODO: check this before parsing routes. or is laziness okay?
    guard $ ext == extString (Proxy @".md")
    s :: Text <- fmap decodeUtf8 $ readFileBS $ fp
    (r,) <$> Pandoc.readCommonMark opts s

-- either or
instance IsPandocExt ('[] :: [Symbol]) where
  readExtFile _ _ _ = pure Nothing

instance (IsPandocExt ext, IsPandocExt exts) => IsPandocExt (ext ': exts) where
  readExtFile _ fp opts = do
    m <- readExtFile (Proxy @ext) fp opts
    case m of
      Nothing -> readExtFile (Proxy @exts) fp opts
      Just x -> pure $ Just x

-- | Represents the relative path to a source .md file
newtype PandocRoute (exts :: [Symbol]) = PandocRoute {unPandocRoute :: NonEmpty Slug}
  deriving stock (Eq, Ord, Show, Generic)

instance ToExts exts => IsString (PandocRoute exts) where
  fromString fp = maybe (error $ "Not a Pandoc source file: " <> toText fp) snd $ mkPandocRoute fp

mkPandocRoute :: forall exts. ToExts exts => FilePath -> Maybe (String, PandocRoute exts)
mkPandocRoute (splitExtension -> (fp, ext)) = do
  r <-
    foldl' (<|>) Nothing $
      toExts @exts Proxy <&> \extCandidate -> do
        guard $ extCandidate == ext
        let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath fp
        viaNonEmpty PandocRoute slugs
  pure (ext, r)

data Model exts = Model
  { modelArg :: Arg
  , modelPandocs :: Map (PandocRoute exts) Pandoc
  }
  deriving stock (Show, Generic)

data Arg = Arg
  { argBaseDir :: FilePath
  , argReaderOpts :: Pandoc.ReaderOptions
  , argWriterOpts :: Pandoc.WriterOptions
  }
  deriving stock (Show, Generic)

instance Default Arg where
  def = Arg "." defaultReaderOpts defaultWriterOpts
    where
      defaultReaderOpts = def {Pandoc.readerExtensions = exts}
      defaultWriterOpts = def {Pandoc.writerExtensions = exts}
      exts :: Pandoc.Extensions
      exts =
        mconcat
          [ Pandoc.extensionsFromList
              [ Pandoc.Ext_fenced_code_attributes
              , Pandoc.Ext_auto_identifiers
              , Pandoc.Ext_smart
              ]
          , Pandoc.githubMarkdownExtensions
          ]

instance IsRoute (PandocRoute exts) where
  type RouteModel (PandocRoute exts) = Model exts
  routePrism m =
    let encode (PandocRoute slugs) =
          toString $ T.intercalate "/" $ Slug.unSlug <$> toList slugs
        decode fp = do
          guard $ not $ null fp
          slugs <- nonEmpty $ fromString . toString <$> T.splitOn "/" (toText fp)
          let r = PandocRoute slugs
          guard $ Map.member r $ modelPandocs m
          pure r
     in toPrism_ $ htmlSuffixPrism % prism' encode decode
  routeUniverse =
    Map.keys . modelPandocs

newtype PandocHtml = PandocHtml {unPandocHtml :: Text}
  deriving stock (Eq, Generic)

instance (ToExts exts, IsPandocExt exts) => EmaSite (PandocRoute exts) where
  type SiteArg (PandocRoute exts) = Arg

  -- Returns the `Pandoc` AST along with the function that renders it to HTML.
  type SiteOutput (PandocRoute exts) = (Pandoc, Pandoc -> PandocHtml)

  siteInput _ arg = do
    docsDyn <- markdownFilesDyn @exts (argBaseDir arg) (argReaderOpts arg)
    pure $ Model arg <$> docsDyn
  siteOutput _ model r = do
    pandoc <- maybe (liftIO $ throwIO $ PandocError_Missing $ coerce r) pure $ Map.lookup r (modelPandocs model)
    pure (pandoc, PandocHtml . renderHtml (argWriterOpts $ modelArg model))

markdownFilesDyn ::
  forall exts m.
  (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m, ToExts exts, IsPandocExt exts) =>
  FilePath ->
  ReaderOptions ->
  m (Dynamic m (Map (PandocRoute exts) Pandoc))
markdownFilesDyn baseDir readerOpts = do
  let pats = [((), "**/*.md")]
      ignorePats = [".*"]
      model0 = mempty
  Dynamic <$> UnionMount.mount baseDir pats ignorePats model0 (const handleUpdate)
  where
    -- Take the file that got changed and update our in-memory `Model` accordingly.
    handleUpdate ::
      (MonadIO m, MonadLogger m, MonadLoggerIO m) =>
      FilePath ->
      UnionMount.FileAction () ->
      m (Map (PandocRoute exts) Pandoc -> Map (PandocRoute exts) Pandoc)
    handleUpdate fp = \case
      UnionMount.Refresh _ _ -> do
        mData <- readSource fp
        pure $ maybe id (uncurry Map.insert) mData
      UnionMount.Delete ->
        pure $ maybe id (Map.delete . snd) $ mkPandocRoute fp
    readSource :: (MonadIO m, MonadLogger m, MonadLoggerIO m) => FilePath -> m (Maybe ((PandocRoute exts), Pandoc))
    readSource fp = runMaybeT $ do
      logD $ "Reading " <> toText fp
      -- TODO: try all exts in @exts
      eRes <- MaybeT $ fmap pure $ liftIO $ runIO $ readExtFile (Proxy @exts) (baseDir </> fp) readerOpts
      case eRes of
        Left err -> Ema.CLI.crash "PandocRoute" $ show err
        Right (Just (r, doc)) ->
          pure (r, doc)
        Right Nothing ->
          MaybeT $ pure Nothing

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "PandocRoute"

renderHtml :: Pandoc.WriterOptions -> Pandoc -> Text
renderHtml writerSettings pandoc =
  either (throw . PandocError_RenderError . show) id $
    Pandoc.runPure $ Pandoc.writeHtml5String writerSettings pandoc

data PandocError = PandocError_Missing (PandocRoute '[]) | PandocError_RenderError Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
