{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.Verification (
  type VerifyModels,
  type VerifyRoutes,
) where

import Data.Type.Bool (If, type (||))
import Data.Type.Equality (type (==))
import Ema.Route.Generic.Iso (IsUnwrappedRoute')
import GHC.Generics qualified as GHC
import GHC.TypeLits (Symbol, type (-))
import Type.Errors.Pretty (TypeError, type (%), type (<>))

{- | @VerifyModels model routeModels lookups@ verifies the given @model@ to ensure that there
exists a valid @HasSubModels@ instance for the given combination of (model, routeModels, lookups).
-}
type family VerifyModels model (routeModels :: [Type]) (lookups :: [Type]) :: Constraint where
  VerifyModels m '[] '[] = ()
  VerifyModels m '[] t =
    TypeError
      ("'WithSubModels' has extra unnecessary types: " % "" % "\t" <> t)
  VerifyModels m f '[] =
    TypeError
      ("'WithSubModels' is missing sub-models: " % "" % "\t" <> f)
  VerifyModels model (f ': fs) (Proxy (n :: Nat) ': ss) =
    If
      (HasPositionT n model f)
      (VerifyModels model fs ss)
      ( TypeError
          ( "The product field at index " <> n <> " of '" <> model <> "' is not of expected type:"
              % ""
              % "\t" <> f
              % ""
          )
      )
  VerifyModels model (f ': fs) (Proxy (s :: Symbol) ': ss) =
    If
      (HasFieldT s model f)
      (VerifyModels model fs ss)
      ( TypeError
          ( "The field '" <> s <> "' of '" <> model <> "' is not of expected type:"
              % ""
              % ("\t" <> f)
              % ""
          )
      )
  VerifyModels model (f ': fs) (ty ': ss) =
    -- (ty == model) checks the simple case that (the model ~ the submodel),
    -- because it doesn't necessarily have to have a generic instance in this case.
    -- After that, we can /assume/ the model has a generic instance to allow us to inspect its
    -- structure statically to verify that the correct submodel exists.
    If
      (ty == model || HasTypeT ty model)
      ( If
          (f == ty)
          (VerifyModels model fs ss)
          ( TypeError
              ( "An argument to 'WithSubModels' contains incorrect submodel selector:"
                  % ""
                  % ("\t" <> ty)
                  % ""
                  % "instead of the expected:"
                  % ""
                  % ("\t" <> f)
              )
          )
      )
      ( TypeError
          ( "Type '"
              <> model
              <> "' does not contain a submodel of type:"
                % ""
                % ("\t" <> ty)
                % ""
                % "but it is specified in 'WithSubModels'"
          )
      )

{- | @VerifyRoutes route rep subroutes@ verifies the given @route@ to ensure that there
exists a valid @HasSubRoutes@ instance for @route@ given its @rep@ and the @subroutes@ it is generic-deriving from.

Invariant: rep ~ Code route
-}
type family VerifyRoutes (route :: Type) (rep :: [[Type]]) (subroutes :: [Type]) :: Constraint where
  VerifyRoutes _ '[] '[] = ()
-- Inconsistent lengths
  VerifyRoutes r '[] t =
    TypeError
      ("'WithSubRoutes' has extra unnecessary types: " % "" % "\t" <> t)
  VerifyRoutes r t '[] =
    TypeError
      ( "'WithSubRoutes' is missing subroutes for:"
          % ""
          % ("\t" <> t)
      )
-- Subroute rep is unit
  VerifyRoutes r ('[] ': rs) (() : rs') = VerifyRoutes r rs rs'
  VerifyRoutes r ('[()] ': rs) (() : rs') = VerifyRoutes r rs rs'
  VerifyRoutes r (r' ': rs) (() : rs') =
    TypeError
      ( "A 'WithSubRoutes' entry is '()' instead of the expected: "
          % r'
      )
-- Constructor type ~ Subroute spec
  VerifyRoutes r ('[r'] ': rs) (r' : rs') = VerifyRoutes r rs rs'
-- Constructor type ~ Unwrapped (Subroute spec) as a last-resort assumption
  VerifyRoutes r (r1 ': rs) (r2 ': rs') =
    If
      (r1 `IsUnwrappedRoute'` (GHC.Rep r2 ()))
      (VerifyRoutes r rs rs')
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

-- Type-level proofs of `HasAny` from generic-optics:
-- --------------------------------------------------

{- | Index into the nth field of a single-constructor type, returning its type

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > HasPositionT 1 X  == \bottom -- X must only have one constructor

 > data X = X Int Bool Float deriving GHC.Generic
 > HasPositionT 0 X == \bottom -- Out of bounds
 > HasPositionT 4 X == \bottom -- Out of bounds
 > HasPositionT 2 X == Bool
-}
type family HasPositionT (i :: Nat) (xs :: Type) (f :: Type) :: Bool where
  HasPositionT i t f = HasPositionT' i (GHC.Rep t ()) == f

type family HasPositionT' (i :: Nat) (xs :: Type) :: Type where
  HasPositionT' n (GHC.D1 _ (GHC.C1 _ fields) _) =
    HasPositionT' n (fields ())
  HasPositionT' 1 ((GHC.S1 _ (GHC.K1 _ t) GHC.:*: nxt) _) =
    t
  HasPositionT' n ((GHC.S1 _ _ GHC.:*: nxt) _) =
    HasPositionT' (n - 1) (nxt ())
  HasPositionT' 1 (GHC.S1 _ (GHC.K1 _ t) _) =
    t
  HasPositionT' 0 (GHC.S1 _ _ _) =
    TypeError ("Type rep indexing: generic selector indexing starts at 1" % "")
  HasPositionT' n (GHC.S1 _ _ _) =
    TypeError ("Type rep indexing: out of bounds index " <> n)
  HasPositionT' _ _ =
    TypeError ("Type rep indexing: multiple constructors" % "")

{- | Extract the type of a field of name @s@ from a generic type representation @t@

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > HasFieldT "foo" X == \bottom -- X must have only one constructor

 > data X = X {y :: Int, z :: Bool} deriving GHC.Generic
 > HasFieldT "x" X == \bottom -- No such selector
 > HasFieldT "z" X == Bool
-}
type family HasFieldT (s :: Symbol) (t :: Type) (f :: Type) :: Bool where
  HasFieldT s t f = HasFieldT' s (GHC.Rep t ()) == f

type family HasFieldT' (s :: Symbol) (t :: Type) :: Type where
  HasFieldT' s (GHC.D1 _ (GHC.C1 _ selectors) _) =
    HasFieldT' s (selectors ())
  HasFieldT' s ((GHC.S1 ( 'GHC.MetaSel ( 'Just s') _ _ _) (GHC.K1 _ t) GHC.:*: nxt) _) =
    If (s == s') t (HasFieldT' s (nxt ()))
  HasFieldT' s (GHC.S1 ( 'GHC.MetaSel ( 'Just s') _ _ _) (GHC.K1 _ t) _) =
    If (s == s') t (TypeError ("Field selector " <> s % " does not exist."))
  HasFieldT' _ _ =
    TypeError ("Type rep field name lookup: multiple constructors" % "")

{- | Traverses the single-constructor generic type representation of a model @r@ to see if at least one of its
 fields has a sunmodel of type @t@.

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > HasTypeT Int X == 'False -- X must have only one constructor

 > data X = X {y :: Int, z :: Bool} deriving GHC.Generic
 > HasTypeT Bool X == 'True
 > HasTypeT Float X == 'False
-}
type family HasTypeT (t :: Type) (r :: Type) :: Bool where
  HasTypeT t r = HasTypeT' t (GHC.Rep r ())

type family HasTypeT' (t :: Type) (r :: Type) :: Bool where
  HasTypeT' () _ =
    'True
  HasTypeT' t (GHC.D1 _ (GHC.C1 _ fields) _) =
    HasTypeT' t (fields ())
  HasTypeT' t ((GHC.S1 _ (GHC.K1 _ t') GHC.:*: nxt) _) =
    t == t' || HasTypeT' t (nxt ())
  HasTypeT' t (GHC.S1 _ (GHC.K1 _ t') _) =
    t == t'
  HasTypeT' t _ =
    'False
