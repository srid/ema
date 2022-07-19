{-# LANGUAGE UndecidableInstances #-}

-- TODO: Consider moving to Internal.Verification instead?
module Ema.Route.Generic.Verification (
  type VerifyModels,
  type VerifyRoutes,
) where

import Data.Type.Bool (If, type (&&), type (||))
import Data.Type.Equality (type (==))
import GHC.Generics qualified as GHC
import GHC.TypeLits (Symbol, type (-))
import Type.Errors.Pretty (TypeError)
import Type.Errors.Pretty qualified as P

{- | Index into the nth field of a single-constructor generic type rep, returning its type

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > Indexed 1 (GHC.Rep X ()) == \bottom -- X must only have one constructor

 > data X = X Int Bool Float deriving GHC.Generic
 > Indexed 0 (GHC.Rep X ()) == \bottom -- Out of bounds
 > Indexed 4 (GHC.Rep X ()) == \bottom -- Out of bounds
 > Indexed 2 (GHC.Rep X ()) == Bool
-}
type family Indexed (i :: Nat) (xs :: Type) :: Type where
  Indexed n (GHC.D1 _ (GHC.C1 _ fields) _) =
    Indexed n (fields ())
  Indexed 1 ((GHC.S1 _ (GHC.K1 _ t) GHC.:*: nxt) _) =
    t
  Indexed n ((GHC.S1 _ _ GHC.:*: nxt) _) =
    Indexed (n - 1) (nxt ())
  Indexed 1 (GHC.S1 _ (GHC.K1 _ t) _) =
    t
  Indexed 0 (GHC.S1 _ _ _) =
    TypeError ("Type rep indexing: generic selector indexing starts at 1" P.% "")
  Indexed n (GHC.S1 _ _ _) =
    TypeError ("Type rep indexing: out of bounds index " P.<> n)
  Indexed _ _ =
    TypeError ("Type rep indexing: multiple constructors" P.% "")

{- | Extract the type of a field of name @s@ from a generic type representation @t@

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > FieldType "foo" (GHC.Rep X ()) == \bottom -- X must have only one constructor

 > data X = X {y :: Int, z :: Bool} deriving GHC.Generic
 > FieldType "x" (GHC.Rep X ()) == \bottom -- No such selector
 > FieldType "z" (GHC.Rep X ()) == Bool
-}
type family FieldType (s :: Symbol) (t :: Type) :: Type where
  FieldType s (GHC.D1 _ (GHC.C1 _ selectors) _) =
    FieldType s (selectors ())
  FieldType s ((GHC.S1 ( 'GHC.MetaSel ( 'Just s') _ _ _) (GHC.K1 _ t) GHC.:*: nxt) _) =
    If (s == s') t (FieldType s (nxt ()))
  FieldType s (GHC.S1 ( 'GHC.MetaSel ( 'Just s') _ _ _) (GHC.K1 _ t) _) =
    If (s == s') t (TypeError ("Field selector " P.<> s P.% " does not exist."))
  FieldType _ _ =
    TypeError ("Type rep field name lookup: multiple constructors" P.% "")

{- | Traverses the single-constructor generic type representation of a model @r@ to see if at least one of its
 fields has a sunmodel of type @t@.

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > ContainsSubModel Int (GHC.Rep X ()) == 'False -- X must have only one constructor

 > data X = X {y :: Int, z :: Bool} deriving GHC.Generic
 > ContainsSubModel Bool (GHC.Rep X ()) == 'True
 > ContainsSubModel Float (GHC.Rep X ()) == 'False
-}
type family ContainsSubModel (t :: Type) (r :: Type) :: Bool where
  ContainsSubModel () _ =
    'True
  ContainsSubModel t (GHC.D1 _ (GHC.C1 _ fields) _) =
    ContainsSubModel t (fields ())
  ContainsSubModel t ((GHC.S1 _ (GHC.K1 _ t') GHC.:*: nxt) _) =
    t == t' || ContainsSubModel t (nxt ())
  ContainsSubModel t (GHC.S1 _ (GHC.K1 _ t') _) =
    t == t'
  ContainsSubModel t _ =
    'False

type family ModelShapeMismatchError r where
  ModelShapeMismatchError r = TypeError ("WithSubModelss list does not match the shape of " P.<> r)

{- | @VerifyModels model routeModels lookups@ verifies the given @model@ to ensure that there
exists a valid @HasSubModels@ instance for the given combination of (model, routeModels, lookups).
-}
type family VerifyModels model (routeModels :: [Type]) (lookups :: [Type]) :: Constraint where
  VerifyModels m '[] '[] = ()
  VerifyModels m '[] _ = ModelShapeMismatchError m
  VerifyModels m _ '[] = ModelShapeMismatchError m
-- TODO: Does not verify if the model is GHC.Generic, and some models don't have /any/ generic instance to begin with,
-- so if we want to be more robust we can dispatch via class instances. However, the base compiler error should look obvious enough.
  VerifyModels model (f ': fs) (Proxy (n :: Nat) ': ss) =
    If
      (f == Indexed n (GHC.Rep model ()))
      (VerifyModels model fs ss)
      ( TypeError
          ( "Submodel at index " P.<> n P.<> " of " P.<> model
              P.% "Has type:"
              P.% Indexed n (GHC.Rep model ())
              P.% "Subroute specification requires that this instead be:"
              P.% f
          )
      )
-- TODO: Does not verify if the model is GHC.Generic, and some models don't have /any/ generic instance to begin with,
-- so if we want to be more robust we can dispatch via class instances. However, the base compiler error should look obvious enough.
  VerifyModels model (f ': fs) (Proxy (s :: Symbol) ': ss) =
    If
      (f == FieldType s (GHC.Rep model ()))
      (VerifyModels model fs ss)
      ( TypeError
          ( "A field " P.<> s P.<> " of type:"
              P.% f
              P.% "Does not exist in model "
              P.% model
          )
      )
  VerifyModels model (f ': fs) (ty ': ss) =
    -- (ty == model) checks the simple case that (the model ~ the submodel),
    -- because it doesn't necessarily have to have a generic instance in this case.
    -- After that, we can /assume/ the model has a generic instance to allow us to inspect its
    -- structure statically to verify that the correct submodel exists.
    If
      (ty == model || ContainsSubModel ty (GHC.Rep model ()))
      ( If
          (f == ty)
          (VerifyModels model fs ss)
          ( TypeError
              ( "Subroute requires a submodel of type:"
                  P.% f
                  P.% "But WithSubModels list at the same index specifies that this should be:"
                  P.% ty
              )
          )
      )
      ( TypeError
          ( model
              P.% "Is not a supermodel of the model:"
              P.% ty
              P.% "Even though this is required by its WithSubRoutes list."
          )
      )

type family RouteShapeMismatchError r where
  RouteShapeMismatchError r = TypeError ("WithSubRoutes list does not match the shape of " P.<> r)

-- | Attempts to 'unwrap' @r2@ to see if the constructor fields specified by @r1@ match its internal representation 1:1
type family IsUnwrappedRoute (r1 :: [Type]) (r2 :: Type) :: Bool where
-- For routes that derived /stock/ GHC.Generic;
-- TODO: The implementation is a bit overkill here as it checks for all fields, but this could be useful
-- should semantics expand in the future perhaps?
  IsUnwrappedRoute ts (GHC.D1 _ (GHC.C1 _ fields) _) =
    IsUnwrappedRoute ts (fields ())
  IsUnwrappedRoute (t ': '[]) (GHC.S1 _ (GHC.K1 _ t') _) =
    t == t'
  IsUnwrappedRoute (t ': ts) ((GHC.S1 _ (GHC.K1 _ t') GHC.:*: nxt) _) =
    t == t' && IsUnwrappedRoute ts (nxt ())
-- Special case for routes with no fields internally, since we can think of Unwrapped () ~ ()
  IsUnwrappedRoute '[] (GHC.U1 ()) =
    'True
-- Catch-all
  IsUnwrappedRoute _ _ =
    'False

-- We need to implement the matching logic as 2 type families here due to overlapping patterns
type family IsUnwrappedRoute' (r1 :: [Type]) (r2 :: Type) :: Bool where
-- For routes that derived /newtype/ GHC.Generic; simply verify the reps are equal
-- Otherwise, pass it on to match with the assumption of /stock/ GHC.Generic deriving
  IsUnwrappedRoute' (t ': ts) ts' =
    GHC.Rep t () == ts' || IsUnwrappedRoute (t ': ts) ts'
  IsUnwrappedRoute' r1 r2 =
    IsUnwrappedRoute r1 r2

{- | @VerifyRoutes route rep subroutes@ verifies the given @route@ to ensure that there
exists a valid @HasSubRoutes@ instance for @route@ given its @rep@ and the @subroutes@ it is generic-deriving from.

Invariant: rep ~ Code route
-}
type family VerifyRoutes (route :: Type) (rep :: [[Type]]) (subroutes :: [Type]) :: Constraint where
  VerifyRoutes _ '[] '[] = ()
-- Inconsistent lengths
  VerifyRoutes r '[] _ = RouteShapeMismatchError r
  VerifyRoutes r _ '[] = RouteShapeMismatchError r
-- Subroute rep is unit
  VerifyRoutes r ('[] ': rs) (() : rs') = VerifyRoutes r rs rs'
  VerifyRoutes r ('[()] ': rs) (() : rs') = VerifyRoutes r rs rs'
  VerifyRoutes r (r' ': rs) (() : rs') =
    TypeError
      ( "WithSubRoutes list states that the route constructor at this index should only contain () or be empty"
          P.% "But it is " P.<> r'
      )
-- Constructor type ~ Subroute spec
  VerifyRoutes r ('[r'] ': rs) (r' : rs') = VerifyRoutes r rs rs'
-- Constructor type ~ Unwrapped (Subroute spec) as a last-resort assumption
-- TODO: Type specified may not be GHC.Generic; might be better to dispatch via class instance.
  VerifyRoutes r (r1 ': rs) (r2 ': rs') =
    If
      (r1 `IsUnwrappedRoute'` (GHC.Rep r2 ()))
      (VerifyRoutes r rs rs')
      ( TypeError
          ( "Route constructor with representation:"
              P.% r1
              P.% "Does not contain a type matching the subroute, or an unwrapped representation of the subroute:"
              P.% r2
              P.% "As specified in its (potentially inferred) WithSubRoutes list."
          )
      )
