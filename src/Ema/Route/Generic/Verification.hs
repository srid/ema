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
      (f == Indexed n model)
      (VerifyModels model fs ss)
      ( TypeError
          ( "The product field at index " <> n <> " of '" <> model <> "' is of type:"
              % ""
              % "\t" <> Indexed n model
              % ""
              % "but in 'WithSubModels' we expect it to be:"
              % ""
              % "\t" <> f
          )
      )
  VerifyModels model (f ': fs) (Proxy (s :: Symbol) ': ss) =
    If
      (f == FieldType s model)
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
      (ty == model || ContainsSubModel ty model)
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

{- | Index into the nth field of a single-constructor type, returning its type

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > Indexed 1 X  == \bottom -- X must only have one constructor

 > data X = X Int Bool Float deriving GHC.Generic
 > Indexed 0 X == \bottom -- Out of bounds
 > Indexed 4 X == \bottom -- Out of bounds
 > Indexed 2 X == Bool
-}
type family Indexed (i :: Nat) (xs :: Type) :: Type where
  Indexed i t = Indexed' i (GHC.Rep t ())

type family Indexed' (i :: Nat) (xs :: Type) :: Type where
  Indexed' n (GHC.D1 _ (GHC.C1 _ fields) _) =
    Indexed' n (fields ())
  Indexed' 1 ((GHC.S1 _ (GHC.K1 _ t) GHC.:*: nxt) _) =
    t
  Indexed' n ((GHC.S1 _ _ GHC.:*: nxt) _) =
    Indexed' (n - 1) (nxt ())
  Indexed' 1 (GHC.S1 _ (GHC.K1 _ t) _) =
    t
  Indexed' 0 (GHC.S1 _ _ _) =
    TypeError ("Type rep indexing: generic selector indexing starts at 1" % "")
  Indexed' n (GHC.S1 _ _ _) =
    TypeError ("Type rep indexing: out of bounds index " <> n)
  Indexed' _ _ =
    TypeError ("Type rep indexing: multiple constructors" % "")

{- | Extract the type of a field of name @s@ from a generic type representation @t@

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > FieldType "foo" X == \bottom -- X must have only one constructor

 > data X = X {y :: Int, z :: Bool} deriving GHC.Generic
 > FieldType "x" X == \bottom -- No such selector
 > FieldType "z" X == Bool
-}
type family FieldType (s :: Symbol) (t :: Type) :: Type where
  FieldType s t = FieldType' s (GHC.Rep t ())

type family FieldType' (s :: Symbol) (t :: Type) :: Type where
  FieldType' s (GHC.D1 _ (GHC.C1 _ selectors) _) =
    FieldType' s (selectors ())
  FieldType' s ((GHC.S1 ( 'GHC.MetaSel ( 'Just s') _ _ _) (GHC.K1 _ t) GHC.:*: nxt) _) =
    If (s == s') t (FieldType' s (nxt ()))
  FieldType' s (GHC.S1 ( 'GHC.MetaSel ( 'Just s') _ _ _) (GHC.K1 _ t) _) =
    If (s == s') t (TypeError ("Field selector " <> s % " does not exist."))
  FieldType' _ _ =
    TypeError ("Type rep field name lookup: multiple constructors" % "")

{- | Traverses the single-constructor generic type representation of a model @r@ to see if at least one of its
 fields has a sunmodel of type @t@.

 Examples:
 > data X = X Int | Y deriving GHC.Generic
 > ContainsSubModel Int X == 'False -- X must have only one constructor

 > data X = X {y :: Int, z :: Bool} deriving GHC.Generic
 > ContainsSubModel Bool X == 'True
 > ContainsSubModel Float X == 'False
-}
type family ContainsSubModel (t :: Type) (r :: Type) :: Bool where
  ContainsSubModel t r = ContainsSubModel' t (GHC.Rep r ())

type family ContainsSubModel' (t :: Type) (r :: Type) :: Bool where
  ContainsSubModel' () _ =
    'True
  ContainsSubModel' t (GHC.D1 _ (GHC.C1 _ fields) _) =
    ContainsSubModel' t (fields ())
  ContainsSubModel' t ((GHC.S1 _ (GHC.K1 _ t') GHC.:*: nxt) _) =
    t == t' || ContainsSubModel' t (nxt ())
  ContainsSubModel' t (GHC.S1 _ (GHC.K1 _ t') _) =
    t == t'
  ContainsSubModel' t _ =
    'False
