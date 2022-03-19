{-# LANGUAGE DeriveAnyClass #-}

-- | Demonstration of merging multiple sites
module Ema.Example.Ex04_Multi where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex01_Basic qualified as Ex01
import Ema.Example.Ex02_Store qualified as Ex02
import Ema.Example.Ex03_Clock qualified as Ex03
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (..), NP (..))
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Generic)

data R
  = R_Index
  | R_Basic Ex01.Route
  | R_Bookshelf Ex02.Route
  | R_Clock Ex03.Route
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute)

type M = NP I '[Ex02.Model, Ex03.Model]

main :: IO ()
main = do
  void $ Ema.runSite @R ()

-- TODO: Generic
instance CanGenerate R where
  generatableRoutes m =
    [R_Index]
      <> (R_Basic <$> generatableRoutes (innerModel m))
      <> (R_Bookshelf <$> generatableRoutes (innerModel m))
      <> (R_Clock <$> generatableRoutes (innerModel m))

instance HasModel R where
  modelDynamic cliAct enc () = do
    x1 :: Dynamic m Ex02.Model <- modelDynamic cliAct (innerRouteEncoder (_As @"R_Bookshelf") enc) ()
    x2 :: Dynamic m Ex03.Model <- modelDynamic cliAct (innerRouteEncoder (_As @"R_Clock") enc) ()
    pure $ liftA2 (\x y -> I x :* I y :* Nil) x1 x2

instance CanRender R where
  routeAsset enc m = \case
    R_Index ->
      Ema.AssetGenerated Ema.Html $ renderIndex m
    R_Basic r ->
      routeAsset (innerRouteEncoder (_As @"R_Basic") enc) (innerModel m) r
    R_Bookshelf r ->
      routeAsset (innerRouteEncoder (_As @"R_Bookshelf") enc) (innerModel m) r
    R_Clock r ->
      routeAsset (innerRouteEncoder (_As @"R_Clock") enc) (innerModel m) r

renderIndex :: M -> LByteString
renderIndex (I _ :* I clockTime :* Nil) =
  tailwindLayout (H.title "MultiSite" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto text-center mt-8 p-2" $ do
      H.p "You can compose Ema sites. Here are two sites composed to produce one:"
      H.ul ! A.class_ "flex flex-col justify-center .items-center mt-4 space-y-4" $ do
        H.li $ routeElem "basic" "Ex01_Basic"
        H.li $ routeElem "bookshelf" "Ex02_Store"
        H.li $ routeElem "clock" "Ex03_Clock"
      H.p $ do
        "The current time is: "
        H.small $ show clockTime
  where
    routeElem url w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! A.href url $ w
