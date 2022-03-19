{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TODO: rewrite this to load books.json and display that, with individual page for books too.
module Ema.Example.Ex02_Bookshelf where

import Data.Text qualified as T
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Route.Encoder
import Generics.SOP qualified as SOP
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Model = Model {modelProducts :: [Text]}

data Route
  = Route_Index
  | Route_About
  | Route_Products ProductRoute
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model Route)

instance HasModel Route where
  modelDynamic _ _ _ = do
    pure $ pure $ Model ["egg", "sausage", "bacon"]

-- TODO: Use DerivingVia to specify options, to disable extra /product/ in URL.
data ProductRoute
  = ProductRoute_Index
  | ProductRoute_Product ProductName
  | ProductRoute_Product2 ProductName
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model ProductRoute)

newtype ProductName = ProductName Text
  deriving stock (Show, Eq)
  deriving newtype (IsString, ToString)

instance IsRoute ProductName where
  type RouteModel ProductName = Model
  mkRouteEncoder =
    unsafeMkRouteEncoder (const $ (<> ".html") . toString) dec
    where
      dec :: Model -> FilePath -> Maybe ProductName
      dec (Model ps) fp = do
        p <- T.stripSuffix ".html" $ toText fp
        guard $ p `elem` ps
        pure $ fromString . toString $ p

instance CanGenerate ProductName where
  generatableRoutes m = ProductName <$> modelProducts m

-- TODO: Generic!
instance CanGenerate ProductRoute where
  generatableRoutes m = [ProductRoute_Index] <> (ProductRoute_Product <$> generatableRoutes m)

instance CanGenerate Route where
  generatableRoutes m = [Route_Index, Route_About] <> (Route_Products <$> generatableRoutes m)

main :: IO ()
main = void $ Ema.runSite @Route ()

instance CanRender Route where
  routeAsset enc m@(Model ps) r =
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
    where
      routeElem r' w =
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl enc m r')
