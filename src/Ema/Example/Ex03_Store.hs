{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A simple web store for products
module Ema.Example.Ex03_Store where

import Control.Concurrent (readChan)
import Control.Exception (throwIO)
import Control.Monad.Logger (MonadLogger, logInfoNS)
import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.SOP (I (I), NP (Nil, (:*)))
import Data.Text qualified as T
import Ema
import Ema.Example.Common (tailwindLayout, watchDirForked)
import Ema.Route.Encoder
import Generics.SOP qualified as SOP
import Optics.Core (Iso', iso)
import System.FSNotify qualified as FSNotify
import System.FilePath (takeFileName, (</>))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Product)

data Model = Model
  { modelStoreName :: Text
  , modelProducts :: [Product]
  , modelCategories :: [Category]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Product = Product {unProduct :: Text}
  deriving newtype (Show, Eq, Ord, IsString, ToString, FromJSON)

newtype Category = Category {unCategory :: Text}
  deriving newtype (Show, Eq, Ord, IsString, ToString, FromJSON)

data Route
  = Route_Index
  | Route_About
  | Route_Products ProductRoute
  | Route_Category CategoryRoute
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (HasSubRoutes)
  deriving (IsRoute) via (Route `WithModel` Model)

-- TODO: Can we derive this automatically?
instance HasSubModels Route where
  subModels m =
    I () :* I () :* I (modelProducts m) :* I (modelCategories m) :* Nil

data ProductRoute
  = ProductRoute_Index
  | ProductRoute_Product Product
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasSubRoutes) via (ProductRoute `WithSubRoutes` '[FileRoute "index.html", Product])
  deriving (IsRoute, HasSubModels) via (ProductRoute `WithConstModel` [Product])

data CategoryRoute
  = CategoryRoute_Index
  | CategoryRoute_Category Category
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasSubRoutes) via (CategoryRoute `WithSubRoutes` '[FileRoute "index.html", Category])
  deriving (IsRoute, HasSubModels) via (CategoryRoute `WithConstModel` [Category])

-- TODO: Replace this with a custom MotleyRoute delgatiion (to `IsString a => StringListRoute a`?)
-- This way we can use the slugify behaviour here as well.
instance IsRoute Product where
  type RouteModel Product = [Product]
  routeEncoder = stringRouteEncoder
  allRoutes = id

instance IsRoute Category where
  type RouteModel Category = [Category]
  routeEncoder =
    stringRouteEncoder
      -- Since category names can contain whitespace, we replace them in URLs
      -- with '-'. This allows use to use stringRouteEncoder, and "fix" up its
      -- result manually.
      & slugifyRouteEncoder
    where
      slugifyRouteEncoder :: RouteEncoder a r -> RouteEncoder a r
      slugifyRouteEncoder =
        mapRouteEncoderFilePath (replacing " " "-")
      replacing :: Text -> Text -> Iso' FilePath FilePath
      replacing needle replacement =
        iso
          (toString . T.replace replacement needle . toText)
          (toString . T.replace needle replacement . toText)
  allRoutes = id

main :: IO ()
main = void $ Ema.runSite @Route ()

instance EmaSite Route where
  siteInput _ () = do
    store0 <- readStoreFile
    pure . Dynamic . (store0,) $ \setModel -> do
      ch <- liftIO $ watchDirForked dataDir
      let loop = do
            log "Waiting for fs event ..."
            evt <- liftIO $ readChan ch
            log $ "Got fs event: " <> show evt
            when (takeFileName (FSNotify.eventPath evt) == "store.json") $ do
              setModel =<< readStoreFile
            loop
      loop
    where
      dataDir = "src/Ema/Example/Ex03_Store"
      readStoreFile :: (MonadIO m, MonadLogger m) => m Model
      readStoreFile = do
        log "Reading Store file"
        liftIO (eitherDecodeFileStrict' $ dataDir </> "store.json") >>= \case
          Left err -> liftIO $ throwIO $ StoreFileMalformed err
          Right store -> pure store
      log :: MonadLogger m => Text -> m ()
      log = logInfoNS "Ex03_Store"
  siteOutput rp (Model storeName ps cats) r =
    Ema.AssetGenerated Ema.Html $
      tailwindLayout (H.title ("Store example: " <> H.toHtml storeName) >> H.base ! A.href "/") $
        H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
          H.h1 ! A.class_ "text-3xl font-bold" $ H.toHtml storeName
          case r of
            Route_Index -> do
              "You are on the index page. "
              routeElem Route_About "Go to About"
              " or go to "
              routeElem (Route_Products ProductRoute_Index) "products"
              " or go to "
              routeElem (Route_Category CategoryRoute_Index) "categories"
            Route_About -> do
              routeElem Route_Index "Go to Index"
              ". You are on the about page. "
            Route_Products pr -> do
              H.h2 "Products"
              case pr of
                ProductRoute_Index -> do
                  H.p "List of products go here"
                  forM_ ps $ \(Product p) -> do
                    H.li $ routeElem (Route_Products $ ProductRoute_Product $ fromString . toString $ p) $ H.toHtml p
                  routeElem Route_Index "Back to index"
                ProductRoute_Product name -> do
                  H.h3 ! A.class_ "p-2 border-2" $ fromString . toString $ name
                  routeElem (Route_Products ProductRoute_Index) "Back to products"
            Route_Category cr -> do
              H.h2 "Categories"
              case cr of
                CategoryRoute_Index -> do
                  H.p "List of categories go here"
                  forM_ cats $ \(Category c) -> do
                    H.li $ routeElem (Route_Category $ CategoryRoute_Category $ fromString . toString $ c) $ H.toHtml c
                  routeElem Route_Index "Back to index"
                CategoryRoute_Category name -> do
                  H.h3 ! A.class_ "p-2 border-2" $ fromString . toString $ name
                  routeElem (Route_Category CategoryRoute_Index) "Back to categories"
    where
      routeElem r' w = do
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl rp r')

newtype StoreFileError = StoreFileMalformed String
  deriving stock (Show, Eq)
  deriving anyclass (Exception)
