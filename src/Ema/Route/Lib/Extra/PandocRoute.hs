{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Lib.Extra.PandocRoute (
  -- * Route
  PandocRoute (..),
  mkPandocRoute,

  -- * Model and Arg
  Model (..),
  Arg (..),

  -- * Looking up Pandoc values in model
  lookupPandocRoute,

  -- * Rendering
  PandocHtml (..),
  PandocError (..),
) where

import Control.Exception (throw, throwIO)
import Control.Monad.Logger (
  MonadLogger,
  MonadLoggerIO,
  logInfoNS,
 )
import Data.Default (Default (..))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.SOP (I (I), NP (..))
import Data.Set qualified as Set
import Ema
import Ema.CLI qualified
import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.SlugRoute
import Generics.SOP qualified as SOP
import System.FilePath ((</>))
import System.UnionMount qualified as UnionMount
import Text.Pandoc (Pandoc, ReaderOptions, runIO)
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Sources (ToSources)
import UnliftIO (MonadUnliftIO)

-- | Represents the relative path to a file that pandoc can parse.
newtype PandocRoute = PandocRoute {unPandocRoute :: SlugRoute Pandoc}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, IsRoute)
    via ( GenericRoute
            PandocRoute
            '[ WithModel Model
             , WithSubRoutes
                '[ SlugRoute Pandoc
                 ]
             ]
        )

instance HasSubModels PandocRoute where
  subModels m = I (Map.mapKeys unPandocRoute $ modelPandocs m) :* Nil

instance IsString PandocRoute where
  fromString fp = maybe (error $ "Bad path: " <> toText fp) snd $ mkPandocRoute fp

mkPandocRoute :: FilePath -> Maybe (String, PandocRoute)
mkPandocRoute fp = do
  (ext, r) <- mkSlugRoute fp
  pure (ext, PandocRoute r)

data Model = Model
  { modelArg :: Arg
  , modelPandocs :: Map PandocRoute Pandoc
  }
  deriving stock (Generic)

lookupPandocRoute :: Model -> PandocRoute -> Maybe (Pandoc, Pandoc -> PandocHtml)
lookupPandocRoute model r = do
  pandoc <- Map.lookup r $ modelPandocs model
  let render = PandocHtml . renderHtml (argWriterOpts $ modelArg model)
  pure (pandoc, render)
  where
    renderHtml :: HasCallStack => Pandoc.WriterOptions -> Pandoc -> Text
    renderHtml writerSettings pandoc =
      either (throw . PandocError_RenderError . show) id $
        Pandoc.runPure $ Pandoc.writeHtml5String writerSettings pandoc

data Arg = Arg
  { -- Base directory
    argBaseDir :: FilePath
  , -- Pandoc reader formats supported, as file extension; eg: '.md'
    argFormats :: Set String
  , argReaderOpts :: Pandoc.ReaderOptions
  , argWriterOpts :: Pandoc.WriterOptions
  }
  deriving stock (Generic)

instance Default Arg where
  def = Arg "." formats defaultReaderOpts defaultWriterOpts
    where
      formats = Set.fromList knownPandocFormats
      defaultReaderOpts = def {Pandoc.readerExtensions = exts}
      defaultWriterOpts = def {Pandoc.writerExtensions = exts}
      exts :: Pandoc.Extensions
      exts =
        -- Sensible defaults for Markdown and others
        Pandoc.pandocExtensions <> Pandoc.extensionsFromList [Pandoc.Ext_attributes]

instance EmaSite PandocRoute where
  type SiteArg PandocRoute = Arg

  -- Returns the `Pandoc` AST along with the function that renders it to HTML.
  type SiteOutput PandocRoute = (Pandoc, Pandoc -> PandocHtml)

  siteInput _ arg = do
    fmap (Model arg) <$> pandocFilesDyn (argBaseDir arg) (argFormats arg) (argReaderOpts arg)
  siteOutput _rp model r = do
    maybe (liftIO $ throwIO $ PandocError_Missing $ show r) pure $ lookupPandocRoute model r

pandocFilesDyn ::
  forall m.
  (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m) =>
  FilePath ->
  Set String ->
  ReaderOptions ->
  m (Dynamic m (Map PandocRoute Pandoc))
pandocFilesDyn baseDir formats readerOpts = do
  let pats =
        toList formats <&> \ext ->
          ((), "**/*" <> ext)
      ignorePats = [".*"]
      model0 = mempty
  Dynamic <$> UnionMount.mount baseDir pats ignorePats model0 (const handleUpdate)
  where
    -- Take the file that got changed and update our in-memory `Model` accordingly.
    handleUpdate ::
      (MonadIO m, MonadLogger m, MonadLoggerIO m) =>
      FilePath ->
      UnionMount.FileAction () ->
      m (Map PandocRoute Pandoc -> Map PandocRoute Pandoc)
    handleUpdate fp = \case
      UnionMount.Refresh _ _ -> do
        mData <- readSource fp
        pure $ maybe id (uncurry Map.insert) mData
      UnionMount.Delete ->
        pure $ maybe id (Map.delete . snd) $ mkPandocRoute fp
    readSource :: (MonadIO m, MonadLogger m, MonadLoggerIO m) => FilePath -> m (Maybe (PandocRoute, Pandoc))
    readSource fp = runMaybeT $ do
      (ext, r :: PandocRoute) <- hoistMaybe (mkPandocRoute fp)
      guard $ ext `Set.member` formats
      log $ "Reading " <> toText fp
      s :: Text <- fmap decodeUtf8 $ readFileBS $ baseDir </> fp
      liftIO (runIO $ readPandocSource readerOpts ext s) >>= \case
        Left err -> Ema.CLI.crash "PandocRoute" $ show err
        Right doc -> do
          pure (r, doc)

log :: MonadLogger m => Text -> m ()
log = logInfoNS "PandocRoute"

data PandocError
  = PandocError_Missing Text
  | PandocError_RenderError Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype PandocHtml = PandocHtml {unPandocHtml :: Text}
  deriving stock (Eq, Generic)

-- Pandoc reader abstraction
--
-- TODO: Can we refactor this using,
-- https://github.com/jgm/pandoc/blob/16f0316fbaa4d667ba40772969ab8e28fea6a493/src/Text/Pandoc/App/FormatHeuristics.hs#L36

knownPandocFormats :: [String]
knownPandocFormats = [".md", ".org"]

formatFromExt :: String -> Text
formatFromExt = \case
  ".md" -> "markdown"
  ".org" -> "org"
  ext -> throw $ UnsupportedPandocFormat ext

readPandocSource ::
  forall m a.
  (Pandoc.PandocMonad m, ToSources a) =>
  ReaderOptions ->
  [Char] ->
  a ->
  m Pandoc
readPandocSource readerOpts ext s =
  case List.lookup (formatFromExt ext) Pandoc.readers of
    Just (Pandoc.TextReader f) -> f readerOpts s
    _ -> throw $ UnsupportedPandocFormat ext

data UnsupportedPandocFormat = UnsupportedPandocFormat String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
