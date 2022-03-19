{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A simple web store for products
--
-- TODO: rewrite this to load store.json and display that, with individual page for books too.
module Ema.Example.Ex02_Store where

import Data.Text qualified as T
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Route.Encoder
import Generics.SOP qualified as SOP
import Optics.Core (Prism', prism')
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Model = Model
  { modelProducts :: [Text],
    modelCategories :: [Text]
  }

data Route
  = Route_Index
  | Route_About
  | Route_Products ProductRoute
  | Route_Category CategoryRoute
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model Route)

instance HasModel Route where
  modelDynamic _ _ _ = do
    pure $ pure $ Model ["egg", "sausage", "bacon"] ["Fresh Meat", "Processed Meat"]

-- TODO: Use DerivingVia to specify options, to disable extra /product/ in URL.
data ProductRoute
  = ProductRoute_Index
  | ProductRoute_Product ProductName
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model ProductRoute)

data CategoryRoute
  = CategoryRoute_Index
  | CategoryRoute_Category CategoryName
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model CategoryRoute)

newtype ProductName = ProductName Text
  deriving stock (Show, Eq)
  deriving newtype (IsString, ToString)

instance IsRoute ProductName where
  type RouteModel ProductName = Model
  mkRouteEncoder =
    stringRouteEncoder
      & anyModelRouteEncoder

instance CanGenerate ProductName where
  generatableRoutes m = ProductName <$> modelProducts m

newtype CategoryName = CategoryName Text
  deriving stock (Show, Eq)
  deriving newtype (IsString, ToString)

instance IsRoute CategoryName where
  type RouteModel CategoryName = Model
  mkRouteEncoder =
    stringRouteEncoder
      & anyModelRouteEncoder
      -- Since category names can contain whitespace, we replace them in URLs
      -- with '-'. This allows use to use stringRouteEncoder, and "fix" up its
      -- result manually.
      & slugifyRouteEncoder
    where
      slugifyRouteEncoder :: RouteEncoder a r -> RouteEncoder a r
      slugifyRouteEncoder =
        mapRouteEncoder
          (replacingPrism " " "-")
          (prism' id Just)
          id
      replacingPrism :: Text -> Text -> Prism' FilePath FilePath
      replacingPrism needle replacement =
        prism'
          (toString . T.replace needle replacement . toText)
          (Just . toString . T.replace replacement needle . toText)

instance CanGenerate CategoryName where
  generatableRoutes m = CategoryName <$> modelCategories m

-- TODO: Generic!
instance CanGenerate ProductRoute where
  generatableRoutes m =
    [ProductRoute_Index] <> (ProductRoute_Product <$> generatableRoutes m)

instance CanGenerate CategoryRoute where
  generatableRoutes m =
    [CategoryRoute_Index] <> (CategoryRoute_Category <$> generatableRoutes m)

instance CanGenerate Route where
  generatableRoutes m =
    [Route_Index, Route_About]
      <> (Route_Products <$> generatableRoutes m)
      <> (Route_Category <$> generatableRoutes m)

main :: IO ()
main = void $ Ema.runSite @Route ()

instance CanRender Route where
  routeAsset enc m@(Model ps cats) r =
    Ema.AssetGenerated Ema.Html $
      tailwindLayout (H.title "Bookshelf site" >> H.base ! A.href "/") $
        H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
          H.h1 ! A.class_ "text-3xl font-bold" $ "TODO: Bookshelf"
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
                  forM_ ps $ \p -> do
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
                  forM_ cats $ \c -> do
                    H.li $ routeElem (Route_Category $ CategoryRoute_Category $ fromString . toString $ c) $ H.toHtml c
                  routeElem Route_Index "Back to index"
                CategoryRoute_Category name -> do
                  H.h3 ! A.class_ "p-2 border-2" $ fromString . toString $ name
                  routeElem (Route_Category CategoryRoute_Index) "Back to categories"
    where
      routeElem r' w =
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl enc m r')
