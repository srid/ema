{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Demonstration of merging multiple sites
module Ema.Example.Ex04_Multi where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex01_Basic qualified as Ex01
import Ema.Example.Ex02_Store qualified as Ex02
import Ema.Example.Ex03_Clock qualified as Ex03
import Ema.Route.Encoder (RouteEncoder)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I), NP (Nil, (:*)))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Generic)

data R
  = R_Index
  | R_Basic Ex01.Route
  | R_Store Ex02.Route
  | R_Clock Ex03.Route
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute)

-- The Generic deriving of `IsRoute` automatically determines the model type.
-- Here we use an alias to refer to it. Note that `Ex01.Route` has an unit model
-- (`()`), and therefore it is not included in this heterogenous list.
type M = NP I '[Ex02.Model, Ex03.Model]

main :: IO ()
main = do
  void $ Ema.runSite @R ()

-- Can we do generic of this too?
-- Can demo in 'mergeSite' (of two Emanotes?)
instance EmaSite R where
  siteInput cliAct enc () = do
    x1 :: Dynamic m Ex02.Model <- siteInput cliAct (innerRouteEncoder (_As @"R_Store") enc) ()
    x2 :: Dynamic m Ex03.Model <- siteInput cliAct (innerRouteEncoder (_As @"R_Clock") enc) ()
    pure $ liftA2 (\x y -> I x :* I y :* Nil) x1 x2
  siteOutput enc m = \case
    R_Index ->
      Ema.AssetGenerated Ema.Html $ renderIndex enc m
    -- Can all of these be generalized? (constructor with 1 encoder; delegate)
    R_Basic r ->
      siteOutput (innerRouteEncoder (_As @"R_Basic") enc) (innerModel m) r
    R_Store r ->
      siteOutput (innerRouteEncoder (_As @"R_Store") enc) (innerModel m) r
    R_Clock r ->
      siteOutput (innerRouteEncoder (_As @"R_Clock") enc) (innerModel m) r

renderIndex :: RouteEncoder M R -> M -> LByteString
renderIndex enc m@(I _store :* I clockTime :* Nil) =
  tailwindLayout (H.title "MultiSite" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto text-center mt-8 p-2" $ do
      H.p "You can compose Ema sites. Here are three sites composed to produce one:"
      H.ul ! A.class_ "flex flex-col justify-center .items-center mt-4 space-y-4" $ do
        H.li $ routeElem (R_Basic Ex01.Route_Index) "Ex01_Basic"
        H.li $ routeElem (R_Store Ex02.Route_Index) "Ex02_Store"
        H.li $ routeElem (R_Clock Ex03.Route_Index) "Ex03_Clock"
      H.p $ do
        "The current time is: "
        -- This illustrates how we can access a sub-model from the top-level
        -- renderer.
        H.small $ show clockTime
  where
    routeElem r w = do
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! A.href (H.toValue $ routeUrl enc m r) $ w
