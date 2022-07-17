module Ema.Route.Lib.File (
  FileRoute (..),
) where

import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (
  mapRoutePrism,
  singletonRoutePrism,
 )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Optics.Core (equality, prism')

{- | A type-level singleton route, whose encoding is given by the symbol parameter.

 FileRoute "foo.html" encodes to "foo.html".

 TODO: Can this type be simplified? See https://stackoverflow.com/q/72755053/55246
-}
data FileRoute (filename :: Symbol) = FileRoute
  deriving stock (Eq, Ord, Show, Generic)

instance KnownSymbol fn => IsRoute (FileRoute fn) where
  type RouteModel (FileRoute fn) = ()
  routePrism =
    singletonRoutePrism (symbolVal (Proxy @fn))
      & mapRoutePrism equality (prism' (const ()) (const $ Just FileRoute)) id
  routeUniverse () =
    [FileRoute]
