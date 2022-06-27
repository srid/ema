{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Most trivial Ema program
module Ema.Example.Ex00_Hello where

import Ema
import Ema.Multi.Generic (WithConstModel (..))
import Ema.Multi.Generic.Motley (HasSubModels, HasSubRoutes)
import Generics.SOP qualified as SOP

data Route = Route_Index
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (HasSubRoutes)
  deriving
    (HasSubModels, IsRoute)
    via (Route `WithConstModel` ())

{- | Without generics, defining IsRoute looks like:

data IndexRoute = IndexRoute
  deriving stock (Show, Eq)

instance IsRoute IndexRoute where
  type RouteModel IndexRoute = ()
  routeEncoder = mkRouteEncoder $ \() ->
    prism'
      (\IndexRoute -> "index.html")
      (\fp -> guard (fp == "index.html") >> pure IndexRoute)
  allRoutes () = [IndexRoute]
-}
instance EmaSite Route where
  siteInput _ _ = pure $ pure ()
  siteOutput _enc _m Route_Index =
    Ema.AssetGenerated Ema.Html "<b>Hello</b>, Ema"

main :: IO ()
main = void $ Ema.runSite @Route ()
