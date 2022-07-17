module Ema.Route.Class (
  IsRoute (RouteModel, routePrism, routeUniverse),
) where

import Ema.Route.Prism (Prism_, toPrism_)
import Optics.Core (only)
import Prelude hiding (All, Generic)

{- | Class of Ema routes

  An Ema route has an encoder, that knows how to convert it to/from filepaths.
  As well as an universe function, `routeUniverse`, that gives all possible route
  values in a static site.

  Both the encoder and the universe function take the associated model as an argument.
-}
class IsRoute r where
  type RouteModel r :: Type

  -- | An optics `Prism'` that denotes how to encode and decode a route.
  routePrism :: RouteModel r -> Prism_ FilePath r

  -- | All possible route values for the given `RouteModel`.
  --
  -- This is used in -- determining the pages to statically generate.
  routeUniverse :: RouteModel r -> [r]

-- Single element routes are represented by `()`
instance IsRoute () where
  type RouteModel () = ()
  routePrism () = toPrism_ $ only "index.html"
  routeUniverse () = one ()
