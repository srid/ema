{-# LANGUAGE UndecidableInstances #-}

-- | Type-level families for @Data.Generics.Product.Any@ from generic-optics
module Ema.Route.Generic.Verification.Any (
  HasAnyT,
) where

import Data.Type.Bool (If, type (&&), type (||))
import Data.Type.Equality (type (==))
import GHC.Generics qualified as GHC
import GHC.TypeLits (Symbol, type (-))
import Type.Errors.Pretty (TypeError, type (%), type (<>))

type family HasAnyT (sel :: Type) (s :: Type) (a :: Type) :: Bool where
  HasAnyT (Proxy (n :: Nat)) s a = HasPositionT n s a
  HasAnyT (Proxy (field :: Symbol)) s a = HasFieldT field s a
  HasAnyT (ty :: Type) s a = HasTypeT ty s a

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
type family HasTypeT (t :: Type) (r :: Type) (f :: Type) :: Bool where
  HasTypeT t r f = HasTypeT' t (GHC.Rep r ()) && t == f

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
