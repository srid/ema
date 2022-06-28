{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Demonstration of merging multiple sites

  For an alternative (easier) approach, see `Ema.Route.Lib.Multi` (used by Ex05_MultiGeneric.hs).
-}
module Ema.Example.Ex04_Multi where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex00_Hello qualified as Ex00
import Ema.Example.Ex01_Basic qualified as Ex01
import Ema.Example.Ex02_Clock qualified as Ex02
import Ema.Example.Ex03_Store qualified as Ex03
import Ema.Route.Encoder (RouteEncoder, mapRouteEncoderModel, mapRouteEncoderRoute)
import Ema.Route.Generic
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I), NP (Nil, (:*)))
import Optics.Core
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Generic)

data M = M
  { mClock :: Ex02.Model
  , mClockFast :: Ex02.Model
  , mStore :: Ex03.Model
  }

data R
  = R_Index
  | R_Hello Ex00.Route
  | R_Basic Ex01.Route
  | R_Clock Ex02.Route
  | R_ClockFast Ex02.Route
  | R_Store Ex03.Route
  deriving stock (Show, Ord, Eq, GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo)
  deriving anyclass (HasSubRoutes)
  deriving (IsRoute) via (R `WithModel` M)

instance HasSubModels R where
  subModels m =
    I () :* I () :* I () :* I (mClock m) :* I (mClockFast m) :* I (mStore m) :* Nil

main :: IO ()
main = do
  void $ Ema.runSite @R ()

instance EmaSite R where
  siteInput cliAct () = do
    x1 :: Dynamic m Ex02.Model <- siteInput @Ex02.Route cliAct Ex02.delayNormal
    x2 :: Dynamic m Ex02.Model <- siteInput @Ex02.Route cliAct Ex02.delayFast
    x3 :: Dynamic m Ex03.Model <- siteInput @Ex03.Route cliAct ()
    pure $ liftA3 M x1 x2 x3
  siteOutput rp m sr =
    let I () :* I m2 :* I m3 :* I m4 :* I m5 :* I m6 :* Nil = subModels @R m
     in case sr of
          R_Index ->
            Ema.AssetGenerated Ema.Html $ renderIndex rp m
          -- TODO: Can all of these be generalized? (constructor with 1 encoder; delegate)
          R_Hello r ->
            -- TODO: add `subSiteOutput enc m (_As @"R_Hello") r`?
            siteOutput (rp % (_As @"R_Hello")) m2 r
          R_Basic r ->
            siteOutput (rp % (_As @"R_Basic")) m3 r
          R_Clock r ->
            siteOutput (rp % (_As @"R_Clock")) m4 r
          R_ClockFast r ->
            siteOutput (rp % (_As @"R_Clock")) m5 r
          R_Store r ->
            siteOutput (rp % (_As @"R_Store")) m6 r

{- | Return the inner `RouteEncoder` corresponding to the route `i`, assuming
 that the model value `a` is known ahead of time.
-}
innerEncoderWithModel ::
  forall (pr :: OpticKind) o i a b.
  pr `Is` A_Prism =>
  a ->
  Optic' pr NoIx o i ->
  RouteEncoder a o ->
  RouteEncoder b i
innerEncoderWithModel m innerRoute enc =
  enc
    & mapRouteEncoderRoute innerRoute
    & mapRouteEncoderModel (\_ -> m) -- TODO: this is wrong; why discard inner values?

renderIndex :: Prism' FilePath R -> M -> LByteString
renderIndex rp m =
  tailwindLayout (H.title "Ex04_Multi" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto text-center mt-8 p-2" $ do
      H.p "You can compose Ema sites. Here are three sites composed to produce one:"
      H.ul ! A.class_ "flex flex-col justify-center .items-center mt-4 space-y-4" $ do
        H.li $ routeElem (R_Hello Ex00.Route_Index) "Ex00_Hello"
        H.li $ routeElem (R_Basic Ex01.Route_Index) "Ex01_Basic"
        H.li $ routeElem (R_Clock Ex02.Route_Index) "Ex02_Clock"
        H.li $ routeElem (R_ClockFast Ex02.Route_Index) "Ex02_ClockFast"
        H.li $ routeElem (R_Store Ex03.Route_Index) "Ex03_Store"
      H.p $ do
        "The current time is: "
        -- This illustrates how we can access a sub-model from the top-level
        -- renderer.
        H.small $ show $ mClock m
  where
    routeElem r w = do
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! A.href (H.toValue $ routeUrl rp r) $ w
