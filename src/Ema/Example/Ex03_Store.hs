{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A simple web store for products
module Ema.Example.Ex03_Store where

import Control.Concurrent (readChan)
import Control.Exception (throwIO)
import Control.Monad.Logger (MonadLogger, logInfoNS)
import Data.Aeson (FromJSON, FromJSONKey, eitherDecodeFileStrict')
import Data.Map.Strict qualified as Map
import Ema
import Ema.Example.Common (tailwindLayout, watchDirForked)
import Ema.Route.Encoder
import Ema.Route.Generic
import Generics.SOP qualified as SOP
import Optics.Core (coercedTo, iso, prism', (%))
import System.FSNotify qualified as FSNotify
import System.FilePath (takeFileName, (</>))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Product)

data Model = Model
  { modelStoreName :: Text
  , modelProducts :: Map Slug Product
  , modelCategories :: Map Slug Category
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Slug = Slug Text
  deriving newtype (Show, Eq, Ord, IsString, ToString, FromJSON, FromJSONKey)
  deriving stock (Generic)

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
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via (GenericRoute Route '[WithModel Model])

data ProductRoute
  = ProductRoute_Index
  | ProductRoute_Product Slug
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            ProductRoute
            '[ WithModel (Map Slug Product)
             , WithSubRoutes
                '[ FileRoute "index.html"
                 , StringRoute Product Slug
                 ]
             ]
        )

data CategoryRoute
  = CategoryRoute_Index
  | CategoryRoute_Category Slug
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            CategoryRoute
            '[ WithModel (Map Slug Category)
             , WithSubRoutes
                '[ FileRoute "index.html"
                 , StringRoute Category Slug
                 ]
             ]
        )

-- | A route represented by a stringy type; associated with a foldable of the same as its model.
newtype StringRoute (a :: Type) r = StringRoute {unStringRoute :: r}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (IsString r, ToString r, Eq r, Ord r) => IsRoute (StringRoute a r) where
  type RouteModel (StringRoute a r) = Map r a
  routeEncoder = mkRouteEncoder \(as :: Map r a) ->
    htmlSuffixPrism
      % iso fromString toString
      % mapMemberPrism as
      % coercedTo
    where
      mapMemberPrism m =
        prism' id $ \r -> do pure r <* (guard $ r `Map.member` m)
  allRoutes as = StringRoute <$> Map.keys as

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
                  forM_ (Map.toList ps) $ \(k, Product p) -> do
                    H.li $ routeElem (Route_Products $ ProductRoute_Product k) $ H.toHtml p
                  routeElem Route_Index "Back to index"
                ProductRoute_Product name -> do
                  H.h3 ! A.class_ "p-2 border-2" $ show $ Map.lookup name ps
                  routeElem (Route_Products ProductRoute_Index) "Back to products"
            Route_Category cr -> do
              H.h2 "Categories"
              case cr of
                CategoryRoute_Index -> do
                  H.p "List of categories go here"
                  forM_ (Map.toList cats) $ \(k, Category c) -> do
                    H.li $ routeElem (Route_Category $ CategoryRoute_Category k) $ H.toHtml c
                  routeElem Route_Index "Back to index"
                CategoryRoute_Category name -> do
                  H.h3 ! A.class_ "p-2 border-2" $ show $ Map.lookup name cats
                  routeElem (Route_Category CategoryRoute_Index) "Back to categories"
    where
      routeElem r' w = do
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl rp r')

newtype StoreFileError = StoreFileMalformed String
  deriving stock (Show, Eq)
  deriving anyclass (Exception)
