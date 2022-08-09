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
import UnliftIO (MonadUnliftIO)

-- | Represents the relative path to a source .md file
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
  -- TODO: Improve this error message, and display `exts` in it.
  fromString fp = maybe (error $ "Unsupported by `PandocRoute` exts: " <> toText fp) snd $ mkPandocRoute fp

-- TODO: Add other extensions
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
  pandoc <- Map.lookup r (modelPandocs model)
  pure (pandoc, PandocHtml . renderHtml (argWriterOpts $ modelArg model))
  where
    renderHtml :: HasCallStack => Pandoc.WriterOptions -> Pandoc -> Text
    renderHtml writerSettings pandoc =
      either (throw . PandocError_RenderError . show) id $
        Pandoc.runPure $ Pandoc.writeHtml5String writerSettings pandoc

data Arg = Arg
  { argBaseDir :: FilePath
  , argFormats :: Set String
  , argReaderOpts :: Pandoc.ReaderOptions
  , argWriterOpts :: Pandoc.WriterOptions
  }
  deriving stock (Generic)

instance Default Arg where
  def = Arg "." formats defaultReaderOpts defaultWriterOpts
    where
      formats = Set.fromList ["*.md", "*.org"]
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
      liftIO (runIO $ readPandocSource ext s) >>= \case
        Left err -> Ema.CLI.crash "PandocRoute" $ show err
        Right doc -> do
          log $ "Parsed " <> toText fp
          pure (r, doc)
    readPandocSource ext =
      case ext of
        ".md" -> Pandoc.readCommonMark readerOpts
        ".org" -> Pandoc.readOrg readerOpts
        _ -> throw $ PandocError_UnsupportedExt ext

log :: MonadLogger m => Text -> m ()
log = logInfoNS "PandocRoute"

data PandocError
  = PandocError_Missing Text
  | PandocError_RenderError Text
  | PandocError_UnsupportedExt String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype PandocHtml = PandocHtml {unPandocHtml :: Text}
  deriving stock (Eq, Generic)
