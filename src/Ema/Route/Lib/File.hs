module Ema.Route.Lib.File (
  FileRoute (..),
) where

import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder (
  mapRouteEncoderRoute,
  singletonRouteEncoderFrom,
 )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (prism')

{- | A type-level singleton route, whose encoding is given by the symbol parameter.

 FileRoute "foo.html" encodes to "foo.html".

 TODO: Can this type be simplified? See https://stackoverflow.com/q/72755053/55246
-}
newtype FileRoute (filename :: Symbol) = FileRoute ()
  deriving stock (Eq, Ord, Show)

instance KnownSymbol fn => IsRoute (FileRoute fn) where
  type RouteModel (FileRoute fn) = ()
  routeEncoder =
    singletonRouteEncoderFrom (symbolVal (Proxy @fn))
      & mapRouteEncoderRoute (prism' (const ()) (const $ Just $ FileRoute ()))
  allRoutes () =
    [FileRoute ()]
