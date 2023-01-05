{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.Verification (
  type VerifyModels,
  type VerifyRoutes,
) where

import Data.Generics.Product.Any (HasAny)
import GHC.TypeLits

{- | @VerifyModels model routeModels lookups@ verifies the given @model@ to ensure that there
exists a valid @HasSubModels@ instance for the given combination of (model, routeModels, lookups).
-}
type family VerifyModels model (subModels :: [Type]) (lookups :: [Type]) :: Constraint where
  VerifyModels m '[] '[] = ()
  VerifyModels m '[] t =
    TypeError
      ('Text "'WithSubModels' has extra unnecessary types: " ':$$: 'Text "" ':$$: 'Text "\t" ':<>: 'ShowType t)
  VerifyModels m f '[] =
    TypeError
      ('Text "'WithSubModels' is missing submodel types: " ':$$: 'Text "" ':$$: 'Text "\t" ':<>: 'ShowType f)
  VerifyModels model (model ': fs) (model ': ss) =
    -- This checks the simple case that (the model ~ the submodel),
    -- because it doesn't necessarily have to have a generic instance in this case.
    -- After that, we can /assume/ the model has a generic instance to allow us to inspect its
    -- structure statically to verify that the correct submodel exists.
    VerifyModels model fs ss
  VerifyModels model (subModel ': subModels) (sel ': sels) =
    ( HasAny sel model model subModel subModel
    , VerifyModels model subModels sels
    )

{- | @VerifyRoutes route rep subroutes@ verifies the given @route@ to ensure that there
exists a valid @HasSubRoutes@ instance for @route@ given its @rep@ and the @subroutes@ it is generic-deriving from.

Invariant: code ~ Code route
-}
type family VerifyRoutes (rcode :: [Type]) (subRoutes :: [Type]) :: Constraint where
  VerifyRoutes '[] '[] = ()
  -- Inconsistent lengths
  VerifyRoutes '[] t =
    TypeError
      ('Text "'WithSubRoutes' has extra unnecessary types: " ':$$: 'Text "" ':$$: 'Text "\t" ':<>: 'ShowType t)
  VerifyRoutes t '[] =
    TypeError
      ( 'Text "'WithSubRoutes' is missing subroutes for:"
          ':$$: 'Text ""
          ':$$: ('Text "\t" ':<>: 'ShowType t)
      )
  -- Subroute rep is unit (REVIEW: this case not strictly necessary anymore; should it be removed?)
  VerifyRoutes (() ': rs) (() : rs') = VerifyRoutes rs rs'
  VerifyRoutes (r' ': rs) (() : rs') =
    TypeError
      ( 'Text "A 'WithSubRoutes' entry is '()' instead of the expected: "
          ':$$: 'ShowType r'
      )
  VerifyRoutes (r1 ': rs) (r2 ': rs') = (Coercible r1 r2, VerifyRoutes rs rs')
