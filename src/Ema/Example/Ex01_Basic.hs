{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A very simple site in three parts: route types, `main` and rendering implementation.
module Ema.Example.Ex01_Basic where

import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Multi.Generic (WithConstModel (..))
import Ema.Multi.Generic.Motley (HasSubModels, HasSubRoutes)
import Generics.SOP qualified as SOP
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Route_Index
  | Route_About
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (HasSubRoutes)
  deriving
    (HasSubModels, IsRoute)
    via (Route `WithConstModel` ())

instance EmaSite Route where
  siteInput _ _ = pure $ pure ()
  siteOutput enc m r =
    Ema.AssetGenerated Ema.Html $
      tailwindLayout (H.title "Basic site" >> H.base ! A.href "/") $
        H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
          H.h1 ! A.class_ "text-3xl font-bold" $ "Basic site"
          case r of
            Route_Index -> do
              "You are on the index page. "
              routeElem Route_About "Go to About"
            Route_About -> do
              routeElem Route_Index "Go to Index"
              ". You are on the about page. "
    where
      routeElem r' w = do
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl enc m r')

main :: IO ()
main = void $ Ema.runSite @Route ()
