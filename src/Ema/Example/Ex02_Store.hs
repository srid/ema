{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A simple web store for products

 TODO: rewrite this to load store.json and display that, with individual page for books too.
-}
module Ema.Example.Ex02_Store where

import Data.Text qualified as T
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Route.Encoder
import Generics.SOP qualified as SOP
import Optics.Core (Iso', iso)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Model = Model
  { modelProducts :: [Text]
  , modelCategories :: [Text]
  }

data Route
  = Route_Index
  | Route_About
  | Route_Products ProductRoute
  | Route_Category CategoryRoute
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model Route)

instance HasModel Route where
  modelDynamic _ _ _ = do
    pure $ pure $ Model ["egg", "sausage", "bacon"] ["Fresh Meat", "Processed Meat"]

-- TODO: Use DerivingVia to specify options, to disable extra /product/ in URL.
data ProductRoute
  = ProductRoute_Index
  | ProductRoute_Product ProductName
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model ProductRoute)

data CategoryRoute
  = CategoryRoute_Index
  | CategoryRoute_Category CategoryName
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model CategoryRoute)

newtype ProductName = ProductName Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString, ToString)

instance IsRoute ProductName where
  type RouteModel ProductName = Model
  routeEncoder =
    stringRouteEncoder
      & anyModelRouteEncoder
  generatableRoutes m =
    ProductName <$> modelProducts m

newtype CategoryName = CategoryName Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString, ToString)

instance IsRoute CategoryName where
  type RouteModel CategoryName = Model
  routeEncoder =
    stringRouteEncoder
      & anyModelRouteEncoder
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
  generatableRoutes m =
    CategoryName <$> modelCategories m

main :: IO ()
main = void $ Ema.runSite @Route ()

instance CanRender Route where
  routeAsset enc m@(Model ps cats) r =
    Ema.AssetGenerated Ema.Html $
      tailwindLayout (H.title "Store example (Ema)" >> H.base ! A.href "/") $
        H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
          H.h1 ! A.class_ "text-3xl font-bold" $ "Store"
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
      routeElem r' w = do
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl enc m r')
