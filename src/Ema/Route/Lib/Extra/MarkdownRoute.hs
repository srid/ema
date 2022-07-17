{-# LANGUAGE DeriveAnyClass #-}

module Ema.Route.Lib.Extra.MarkdownRoute (
  -- * Route
  MarkdownRoute (..),
  mkMarkdownRoute,

  -- * Model and Arg
  Model (..),
  Arg (..),

  -- * Rendering
  MarkdownHtml (..),
  MarkdownError (..),
) where

import Commonmark.Simple as Commonmark
import Control.Exception (throw)
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
import Ema.Route.Encoder (htmlSuffixPrism)
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Optics.Core (prism', (%))
import System.FilePath (splitExtension, splitPath, (</>))
import System.UnionMount qualified as UnionMount
import Text.Pandoc (Pandoc)
import Text.Pandoc qualified as Pandoc
import UnliftIO (MonadUnliftIO)

-- | Represents the relative path to a source .md file
newtype MarkdownRoute = MarkdownRoute {unMarkdownRoute :: NonEmpty Slug}
  deriving stock (Eq, Ord, Show, Generic)

instance IsString MarkdownRoute where
  fromString = fromMaybe (error "Not a Markdown file") . mkMarkdownRoute

mkMarkdownRoute :: FilePath -> Maybe MarkdownRoute
mkMarkdownRoute = \case
  (splitExtension -> (fp, ".md")) ->
    let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath fp
     in viaNonEmpty MarkdownRoute slugs
  _ ->
    Nothing

data Model = Model
  { modelArg :: Arg
  , modelPandocs :: Map MarkdownRoute Pandoc
  }
  deriving stock (Show, Generic)

data Arg = Arg
  { argBaseDir :: FilePath
  , argWriterOpts :: Pandoc.WriterOptions
  }
  deriving stock (Show, Generic)

instance Default Arg where
  def = Arg "." defaultWriterOpts
    where
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

instance IsRoute MarkdownRoute where
  type RouteModel MarkdownRoute = Model
  routePrism = mkRoutePrism $ \m ->
    let encode (MarkdownRoute slugs) =
          toString $ T.intercalate "/" $ Slug.unSlug <$> toList slugs
        decode fp = do
          guard $ not $ null fp
          slugs <- nonEmpty $ fromString . toString <$> T.splitOn "/" (toText fp)
          let r = MarkdownRoute slugs
          guard $ Map.member r $ modelPandocs m
          pure r
     in htmlSuffixPrism % prism' encode decode
  routeUniverse =
    Map.keys . modelPandocs

newtype MarkdownHtml = MarkdownHtml {unMarkdownHtml :: Text}
  deriving stock (Eq, Generic)

instance EmaSite MarkdownRoute where
  type SiteArg MarkdownRoute = Arg

  -- Returns the `Pandoc` AST along with the function that renders it to HTML.
  type SiteOutput MarkdownRoute = (Pandoc, Pandoc -> MarkdownHtml)

  siteInput _ arg = do
    docsDyn <- markdownFilesDyn (argBaseDir arg)
    pure $ Model arg <$> docsDyn
  siteOutput _ model r =
    let pandoc = Map.findWithDefault (throw $ MarkdownError_Missing r) r $ modelPandocs model
     in (pandoc, MarkdownHtml . renderHtml (argWriterOpts $ modelArg model))

markdownFilesDyn :: (MonadIO m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m) => FilePath -> m (Dynamic m (Map MarkdownRoute Pandoc))
markdownFilesDyn baseDir = do
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
      m (Map MarkdownRoute Pandoc -> Map MarkdownRoute Pandoc)
    handleUpdate fp = \case
      UnionMount.Refresh _ _ -> do
        mData <- readSource fp
        pure $ maybe id (uncurry Map.insert) mData
      UnionMount.Delete ->
        pure $ maybe id Map.delete $ mkMarkdownRoute fp
    readSource :: (MonadIO m, MonadLogger m, MonadLoggerIO m) => FilePath -> m (Maybe (MarkdownRoute, Pandoc))
    readSource fp = runMaybeT $ do
      r :: MarkdownRoute <- hoistMaybe (mkMarkdownRoute fp)
      logD $ "Reading " <> toText fp
      s <- fmap decodeUtf8 $ readFileBS $ baseDir </> fp
      case Commonmark.parseMarkdownWithFrontMatter @(Map Text Text) Commonmark.fullMarkdownSpec fp s of
        Left err -> Ema.CLI.crash "MarkdownRoute" err -- TODO: propagate errors sensibly (in model)
        Right (_mMeta, doc) -> pure (r, doc)

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "MarkdownRoute"

renderHtml :: Pandoc.WriterOptions -> Pandoc -> Text
renderHtml writerSettings pandoc =
  either (throw . MarkdownError_RenderError . show) id $
    Pandoc.runPure $ Pandoc.writeHtml5String writerSettings pandoc

data MarkdownError = MarkdownError_Missing MarkdownRoute | MarkdownError_RenderError Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
