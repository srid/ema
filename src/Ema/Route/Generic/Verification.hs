{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.Verification (
  type VerifyModels,
  type VerifyRoutes,
) where

import Data.Type.Bool (If)
import Ema.Route.Generic.Verification.Any (HasAnyT)
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
    If
      (HasAnyT sel model subModel)
      (VerifyModels model subModels sels)
      ( TypeError
          ( "The 'WithSubModel' selector " <> sel <> " of '" <> model <> "' is not of expected type:"
              % ""
              % "\t" <> subModel
              % ""
          )
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
      ("'WithSubRoutes' has extra unnecessary types: " % "" % "\t" <> t)
  VerifyRoutes t '[] =
    TypeError
      ( "'WithSubRoutes' is missing subroutes for:"
          % ""
          % ("\t" <> t)
      )
-- Subroute rep is unit (REVIEW: this case not strictly necessary anymore; should it be removed?)
  VerifyRoutes (() ': rs) (() : rs') = VerifyRoutes rs rs'
  VerifyRoutes (r' ': rs) (() : rs') =
    TypeError
      ( "A 'WithSubRoutes' entry is '()' instead of the expected: "
          % r'
      )
  VerifyRoutes (r1 ': rs) (r2 ': rs') = (Coercible r1 r2, VerifyRoutes rs rs')
