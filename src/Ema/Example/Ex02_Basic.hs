{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Route.Generic (IsRoute)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Generic)

data Route
  = Route_Index
  | Route_About
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute, HasModel)

main :: IO ()
main = void $ Ema.runSite @Route ()

instance RenderAsset Route where
  renderAsset enc () r =
    Ema.AssetGenerated Ema.Html $
      tailwindLayout (H.title "Basic site" >> H.base ! A.href "/") $
        H.div ! A.class_ "container mx-auto" $ do
          H.div ! A.class_ "mt-8 p-2 text-center" $ do
            case r of
              Route_Index -> do
                "You are on the index page. "
                routeElem Route_About "Go to About"
              Route_About -> do
                routeElem Route_Index "Go to Index"
                ". You are on the about page. "
    where
      routeElem r' w =
        H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
      routeHref r' =
        A.href (fromString . toString $ Ema.routeUrl enc () r')
