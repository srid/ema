{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Lib.Extra.PandocRoute (
  -- * Route
  PandocRoute (..),
  mkPandocRoute,

  -- * Model and Arg
  Model (..),
  Arg (..),

  -- * Looking up
  lookupPandoc,

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
import Data.Map.Strict qualified as Map
import Data.SOP (I (I), NP (..))
import Ema
import Ema.CLI qualified
import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.SlugRoute
import GHC.TypeLits (Symbol, symbolVal)
import Generics.SOP qualified as SOP
import System.FilePath ((</>))
import System.UnionMount qualified as UnionMount
import Text.Pandoc (Pandoc, PandocMonad, ReaderOptions, runIO)
import Text.Pandoc qualified as Pandoc
import UnliftIO (MonadUnliftIO)

-- TODO: Move Pandoc Ext stuff to separate module

class IsPandocExt (ext :: k) where
  readExtFile ::
    forall (exts :: [Symbol]) m.
    (PandocMonad m, IsSlugRoute Pandoc exts, MonadIO m) =>
    Proxy ext ->
    FilePath ->
    FilePath ->
    ReaderOptions ->
    m (Maybe (PandocRoute exts, Pandoc))

instance IsPandocExt ".md" where
  readExtFile = readExtFile' Pandoc.readCommonMark

readExtFile' ::
  forall ext (exts :: [Symbol]) m.
  (PandocMonad m, IsSlugRoute Pandoc exts, MonadIO m) =>
  (ReaderOptions -> Text -> MaybeT m Pandoc) ->
  Proxy ext ->
  FilePath ->
  FilePath ->
  ReaderOptions ->
  m (Maybe (PandocRoute exts, Pandoc))
readExtFile' f _ baseDir fp opts = runMaybeT $ do
  (ext, r :: PandocRoute exts) <- hoistMaybe (mkPandocRoute fp)
  -- TODO: check this before parsing routes. or is laziness okay?
  guard $ ext == symbolVal (Proxy @".md")
  s :: Text <- fmap decodeUtf8 $ readFileBS $ baseDir </> fp
  (r,) <$> f opts s

instance IsPandocExt ('[] :: [Symbol]) where
  readExtFile _ _ _ _ = pure Nothing

instance (IsPandocExt ext, IsPandocExt exts) => IsPandocExt (ext ': exts) where
  readExtFile _ baseDir fp opts = do
    m <- readExtFile (Proxy @ext) baseDir fp opts
    case m of
      Nothing -> readExtFile (Proxy @exts) baseDir fp opts
      Just x -> pure $ Just x

--
--

-- | Represents the relative path to a source .md file
newtype PandocRoute (exts :: [Symbol]) = PandocRoute {unPandocRoute :: SlugRoute exts Pandoc}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, IsRoute)
    via ( GenericRoute
            (PandocRoute exts)
            '[ WithModel (Model exts)
             , WithSubRoutes
                '[ SlugRoute exts Pandoc
                 ]
             ]
        )

instance HasSubModels (PandocRoute exts) where
  subModels m = I (Map.mapKeys unPandocRoute $ modelPandocs m) :* Nil

deriving newtype instance IsSlugRoute Pandoc exts => IsString (PandocRoute exts)

mkPandocRoute :: IsSlugRoute Pandoc exts => FilePath -> Maybe (String, PandocRoute exts)
mkPandocRoute = fmap (fmap PandocRoute) . mkSlugRoute

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
        -- Sensible defaults for Markdown and others
        Pandoc.pandocExtensions <> Pandoc.extensionsFromList [Pandoc.Ext_attributes]

newtype PandocHtml = PandocHtml {unPandocHtml :: Text}
  deriving stock (Eq, Generic)

lookupPandoc :: forall {exts :: [Symbol]}. Model exts -> PandocRoute exts -> Maybe (Pandoc, Pandoc -> PandocHtml)
lookupPandoc model r = do
  pandoc <- Map.lookup r (modelPandocs model)
  pure (pandoc, PandocHtml . renderHtml (argWriterOpts $ modelArg model))

pandocFilesDyn ::
  forall exts m.
  (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m, IsSlugRoute Pandoc exts, IsPandocExt exts) =>
  FilePath ->
  ReaderOptions ->
  m (Dynamic m (Map (PandocRoute exts) Pandoc))
pandocFilesDyn baseDir readerOpts = do
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
      log $ "Reading " <> toText fp
      -- TODO: try all exts in @exts
      eRes <- MaybeT $ fmap pure $ liftIO $ runIO $ readExtFile (Proxy @exts) baseDir fp readerOpts
      case eRes of
        Left err -> Ema.CLI.crash "PandocRoute" $ show err
        Right (Just (r, doc)) -> do
          log $ "Parsed " <> toText fp
          pure (r, doc)
        Right Nothing ->
          MaybeT $ pure Nothing

log :: MonadLogger m => Text -> m ()
log = logInfoNS "PandocRoute"

renderHtml :: Pandoc.WriterOptions -> Pandoc -> Text
renderHtml writerSettings pandoc =
  either (throw . PandocError_RenderError . show) id $
    Pandoc.runPure $ Pandoc.writeHtml5String writerSettings pandoc

data PandocError = PandocError_Missing (PandocRoute '[]) | PandocError_RenderError Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

instance (IsSlugRoute Pandoc exts, IsPandocExt exts) => EmaSite (PandocRoute exts) where
  type SiteArg (PandocRoute exts) = Arg

  -- Returns the `Pandoc` AST along with the function that renders it to HTML.
  type SiteOutput (PandocRoute exts) = (Pandoc, Pandoc -> PandocHtml)

  siteInput _ arg = do
    docsDyn <- pandocFilesDyn @exts (argBaseDir arg) (argReaderOpts arg)
    pure $ Model arg <$> docsDyn
  siteOutput _ model r = do
    maybe (liftIO $ throwIO $ PandocError_Missing $ coerce r) pure $ lookupPandoc model r
