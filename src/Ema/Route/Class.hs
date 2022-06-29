module Ema.Route.Class (
  IsRoute (RouteModel, routeEncoder, allRoutes),
) where

import Ema.Route.Encoder (RouteEncoder, singletonRouteEncoder)
import Prelude hiding (All, Generic)

{- | Class of Ema routes

  An Ema route has an encoder, that knows how to convert it to/from filepaths.
  As well as an universe function, `allRoutes`, that gives all possible route
  values in a static site.

  Both the encoder and the universe function take the associated model as an argument.
-}
class IsRoute r where
  type RouteModel r :: Type
  routeEncoder :: RouteEncoder (RouteModel r) r
  allRoutes :: RouteModel r -> [r]

-- Single element routes are represented by `()`
instance IsRoute () where
  type RouteModel () = ()
  routeEncoder = singletonRouteEncoder "index.html"
  allRoutes _ = [()]
