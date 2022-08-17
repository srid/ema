{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.Verification (
  type VerifyModels,
  type VerifyRoutes,
) where

import Data.Type.Bool (If)
import Ema.Route.Generic.Iso (IsUnwrappedRoute')
import GHC.Generics qualified as GHC
import Data.Generics.Product.Any (HasAny)
import Type.Errors.Pretty (TypeError, type (%), type (<>))

{- | @VerifyModels model routeModels lookups@ verifies the given @model@ to ensure that there
exists a valid @HasSubModels@ instance for the given combination of (model, routeModels, lookups).
-}
type family VerifyModels model (subModels :: [Type]) (lookups :: [Type]) :: Constraint where
  VerifyModels m '[] '[] = ()
  VerifyModels m '[] t =
    TypeError
      ("'WithSubModels' has extra unnecessary types: " % "" % "\t" <> t)
  VerifyModels m f '[] =
    TypeError
      ("'WithSubModels' is missing submodel types: " % "" % "\t" <> f)
  VerifyModels model (model ': fs) (model ': ss) =
    -- This checks the simple case that (the model ~ the submodel),
    -- because it doesn't necessarily have to have a generic instance in this case.
    -- After that, we can /assume/ the model has a generic instance to allow us to inspect its
    -- structure statically to verify that the correct submodel exists.
    VerifyModels model fs ss
  VerifyModels model (subModel ': subModels) (sel ': sels) =
    ( HasAny sel model model subModel subModel,
      VerifyModels model subModels sels
    )

{- | @VerifyRoutes route rep subroutes@ verifies the given @route@ to ensure that there
exists a valid @HasSubRoutes@ instance for @route@ given its @rep@ and the @subroutes@ it is generic-deriving from.

Invariant: code ~ Code route
-}
type family VerifyRoutes (code :: [[Type]]) (subRoutes :: [Type]) :: Constraint where
  VerifyRoutes '[] '[] = ()
-- Inconsistent lengths
  VerifyRoutes '[] t =
    TypeError
      ("'WithSubRoutes' has extra unnecessary types: " % "" % "\t" <> t)
  VerifyRoutes t '[] =
    TypeError
      ( "'WithSubRoutes' is missing subroutes for:"
          % ""
          % ("\t" <> t)
      )
-- Subroute rep is unit
  VerifyRoutes ('[] ': rs) (() : rs') = VerifyRoutes rs rs'
  VerifyRoutes ('[()] ': rs) (() : rs') = VerifyRoutes rs rs'
  VerifyRoutes (r' ': rs) (() : rs') =
    TypeError
      ( "A 'WithSubRoutes' entry is '()' instead of the expected: "
          % r'
      )
-- Constructor type ~ Subroute spec
  VerifyRoutes ('[r'] ': rs) (r' : rs') = VerifyRoutes rs rs'
-- Constructor type ~ Unwrapped (Subroute spec) as a last-resort assumption
  VerifyRoutes (r1 ': rs) (r2 ': rs') =
    If
      (r1 `IsUnwrappedRoute'` (GHC.Rep r2 ()))
      (VerifyRoutes rs rs')
      ( TypeError
          ( "A 'WithSubRoutes' type:"
              % ""
              % ("\t" <> r2)
              % ""
              % "is not isomorphic to the corresponding route constructor type:"
              % ""
              % ("\t" <> r1)
              % ""
          )
      )
