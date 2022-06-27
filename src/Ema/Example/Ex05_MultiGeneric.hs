{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Demonstration of merging multiple sites using `Ema.Route.Lib.Multi`
module Ema.Example.Ex05_MultiGeneric where

import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex00_Hello qualified as Ex00
import Ema.Example.Ex01_Basic qualified as Ex01
import Ema.Example.Ex02_Clock qualified as Ex02
import Ema.Example.Ex03_Store qualified as Ex03
import Ema.Route.Lib.Multi (MultiRoute)
import Generics.SOP (I (I), NP (Nil, (:*)))
import Generics.SOP qualified as SOP
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

type R =
  MultiRoute
    '[ TopRoute
     , FolderRoute "hello" Ex00.Route
     , FolderRoute "basic" Ex01.Route
     , FolderRoute "clock" Ex02.Route
     , FolderRoute "clockfast" Ex02.Route
     , FolderRoute "store" Ex03.Route
     ]

data TopRoute = TopRoute_Index
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (HasSubRoutes)
  deriving
    (HasSubModels, IsRoute)
    via (TopRoute `WithConstModel` ())

instance EmaSite TopRoute where
  siteInput _ _ = pure $ pure ()
  siteOutput _enc _ TopRoute_Index =
    Ema.AssetGenerated Ema.Html renderIndex

main :: IO ()
main = do
  void $ Ema.runSite @R $ I () :* I () :* I () :* I Ex02.delayNormal :* I Ex02.delayFast :* I () :* Nil

renderIndex :: LByteString
renderIndex =
  tailwindLayout (H.title "Ex05_MultiGeneric" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto text-center mt-8 p-2" $ do
      H.p "You can compose Ema sites. Here are three sites composed to produce one, using Ema.Route.Lib.Multi:"
      H.ul ! A.class_ "flex flex-col justify-center .items-center mt-4 space-y-4" $ do
        -- NOTE: Since we don't have the MultiModel, we must hardcode the routes here.
        H.li $ routeElem "hello" "Ex00_Hello"
        H.li $ routeElem "basic" "Ex01_Basic"
        H.li $ routeElem "clock" "Ex02_Clock"
        H.li $ routeElem "clockfast" "Ex02_Clock (Fast)"
        H.li $ routeElem "store" "Ex03_Store"
  where
    routeElem r w = do
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! A.href r $ w
