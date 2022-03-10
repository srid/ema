{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Demonstration of merging multiple sites
module Ema.Example.Ex04_Multi where

import Control.Lens.Combinators (iso)
import Ema qualified
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex02_Basic qualified as Ex02
import Ema.Example.Ex03_Clock qualified as Ex03
import Ema.Route.Generic
import Ema.Site
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (..), NP (..))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Generic)

data R
  = R_Index
  | R_Basic Ex02.Route
  | R_Clock Ex03.Route
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute)

getBasic :: R -> Maybe Ex02.Route
getBasic = \case
  R_Basic r -> Just r
  _ -> Nothing

getClock :: R -> Maybe Ex03.Route
getClock = \case
  R_Clock r -> Just r
  _ -> Nothing

type M = NP I '[Ex02.Model, Ex03.Model]

main :: IO ()
main = do
  Ema.runSite_ site

-- TODO: make all of the below composable.
site :: Site M R
site =
  Site
    { siteName = "Ex04",
      siteModelManager = ModelManager $ do
        cliAct <- askCLIAction
        enc <- askRouteEncoder
        lift $ do
          x1 <- runModelManager (siteModelManager Ex02.site) cliAct $ pullOutRouteEncoder (iso getBasic R_Basic) enc
          x2 <- runModelManager (siteModelManager Ex03.site) cliAct $ pullOutRouteEncoder (iso getClock R_Clock) enc
          pure $ liftA2 (\a b -> I a :* I b :* Nil) x1 x2
    }

instance RenderAsset R where
  renderAsset enc m r' =
    Ema.AssetGenerated Ema.Html $ case r' of
      R_Index -> renderIndex m
      R_Basic r ->
        let enc' = pullOutRouteEncoder (iso getBasic R_Basic) enc
         in Ex02.render enc' (getModel m) r
      R_Clock r ->
        let enc' = pullOutRouteEncoder (iso getClock R_Clock) enc
         in Ex03.render enc' (getModel m) r

renderIndex :: M -> LByteString
renderIndex (I (Ex02.Model msg) :* I clockTime :* Nil) =
  tailwindLayout (H.title "MultiSite" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto text-center mt-8 p-2" $ do
      H.p "You can compose Ema sites. Here are two sites composed to produce one:"
      H.ul ! A.class_ "flex flex-col justify-center .items-center mt-4 space-y-4" $ do
        H.li $ do
          routeElem "basic" "Ex02_Basic"
          " (" <> H.toHtml msg <> ")"
        H.li $ routeElem "clock" "Ex03_Clock"
      H.p $ do
        "The current time is: "
        H.small $ show clockTime
  where
    routeElem url w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! A.href url $ w
