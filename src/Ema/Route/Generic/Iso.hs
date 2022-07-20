{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.Iso (
  GIsomorphic (giso),
  IsUnwrappedRoute',
) where

import Data.Type.Bool (type (&&), type (||))
import Data.Type.Equality (type (==))
import GHC.Generics qualified as GHC
import Optics.Core (Iso', iso)

{- | Types `a` and `b` are isomorphic via their generic representation

  Unlike `Coercible` this constraint does not require that the two types have
  identical *runtime* representation. For example, `data Foo = Foo` is not
  coercible to `()` (they have different runtime representations), but they are
  both isomorphic via their generic representation.
-}
class (Generic a, Generic b, GHC.Rep a () `Coercible` GHC.Rep b ()) => GIsomorphic a b where
  giso :: Iso' a b

instance (Generic a, Generic b, GHC.Rep a () `Coercible` GHC.Rep b ()) => GIsomorphic a b where
  giso =
    iso
      (GHC.to @b @() . coerce . GHC.from @a @())
      (GHC.to @a @() . coerce . GHC.from @b @())

-- NOTE: The following code was added as part of custom type errors PR
-- https://github.com/EmaApps/ema/pull/120
--
-- It may require further reflection, and even a rewrite. It exists in this
-- module insofar as it checks the `GIsomorphic` constraint at compile-time for
-- deriving clauses.

{- | Attempts to 'unwrap' @r2@ to see if the constructor fields specified by @r1@ match its internal representation 1:1

 In the ideal world, we would piggyback on @GIsoMorphic@ to do the
 heavylifting for us, but GHC is not smart enough for that yet.
-}
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
