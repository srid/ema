{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Helpers for generics-sop
module Data.SOP.Extra where

import Data.SOP.Constraint (SListIN)
import Data.Text qualified as T
import GHC.TypeLits (
  ErrorMessage (ShowType, Text, (:$$:)),
  TypeError,
 )
import Generics.SOP
import Optics.Core
import Prelude hiding (All)

-- | Like `HCollapse`, but limited to 0 or 1 products in a n-ary structure.
class HCollapseMaybe h xs where
  hcollapseMaybe :: SListIN h xs => h (K a) xs -> Maybe a

instance HCollapseMaybe NP '[] where
  hcollapseMaybe Nil = Nothing

instance HCollapseMaybe NP '[p] where
  hcollapseMaybe (K x :* Nil) = Just x

instance (ps ~ TypeError ( 'Text "Expected at most 1 product")) => HCollapseMaybe NP (p ': p1 ': ps) where
  hcollapseMaybe _ = Nothing -- Unreachable, due to TypeError

{- | `x` is contained in `xs`

  Gives a free `Iso` into the `NP I xs`.

  TODO: Can this be simplified?
-}
class NPContains (xs :: [Type]) (x :: Type) where
  -- | A partial iso into/from NP, given a member type. When creating the outer NP structure, rest of the members will be `undefined`.
  npIso :: Iso' (NP I xs) x

there :: Iso' (NP I (x ': xs)) (NP I xs)
there = iso tl (\t -> I willNotBeUsed :* t)

here :: Iso' (NP I (x ': xs)) x
here = iso (unI . hd) (\x -> I x :* willNotBeUsed)

-- FIXME: a design hack necessitates it.
willNotBeUsed :: HasCallStack => a
willNotBeUsed = error "This value will not be used"

-- Could probably replace this lens-sop:
-- with https://hackage.haskell.org/package/lens-sop-0.2.0.3/docs/Generics-SOP-Lens.html#v:np
instance {-# OVERLAPPING #-} NPContains '[] () where
  -- () is always contained in any structure.
  npIso = iso (const ()) (\() -> Nil)
instance {-# OVERLAPPING #-} NPContains '[] (NP I '[]) where
  -- NP I '[] is always contained in any structure.
  npIso = iso (const Nil) id
instance {-# OVERLAPPING #-} NPContains xs () => NPContains (x ': xs) () where
  npIso = there % npIso
instance (TypeError ( 'Text "The type " ':$$: 'ShowType x ':$$: 'Text " does not exist in n-ary product")) => NPContains '[] x where
  npIso = iso willNotBeUsed willNotBeUsed
instance {-# OVERLAPPING #-} NPContains (x ': xs) x where
  npIso = here
instance {-# OVERLAPPABLE #-} NPContains xs x => NPContains (x' ': xs) x where
  npIso = there % npIso

{- | Like `NP` but all elements are the same.

 Each of `xs` is equivalent to `a`.
-}
class NPConst (f :: k -> Type) (xs :: [k]) (a :: k) where
  npConstFrom :: f a -> NP f xs

instance NPConst I '[] a where
  npConstFrom _ = Nil

instance {-# OVERLAPPING #-} NPConst f (x ': '[]) x where
  npConstFrom x = x :* Nil

instance (NPConst f xs x) => NPConst f (x ': xs) x where
  npConstFrom x = x :* npConstFrom @_ @f @xs @x x

datatypeCtors :: forall a. HasDatatypeInfo a => NP ConstructorInfo (Code a)
datatypeCtors = constructorInfo $ datatypeInfo (Proxy @a)

ctorStripPrefix :: forall a. HasDatatypeInfo a => ConstructorName -> String
ctorStripPrefix ctorName =
  let name = datatypeName $ datatypeInfo (Proxy @a)
   in -- TODO: The error message here should be a compile time error
      maybe (error $ toText $ "ctor: bad naming: " <> ctorName) (toString . T.toLower) $ do
        suffix <- T.stripPrefix (toText $ name <> "_") (toText ctorName)
        guard $ not $ T.null suffix -- Disallow `data Foo = Foo_` type constructors, because we don't yet know how to deal with it.
        pure suffix

-- | Like `mcana_NS` but returns a Maybe
mcana_NS ::
  forall c proxy s f xs.
  (All c xs) =>
  proxy c ->
  (forall y ys. c y => s (y ': ys) -> Either (Maybe (f y)) (s ys)) ->
  s xs ->
  Maybe (NS f xs)
mcana_NS _ decide = go sList
  where
    go :: forall ys. (All c ys) => SList ys -> s ys -> Maybe (NS f ys)
    go SNil _ = Nothing
    go SCons s = case decide s of
      Left x -> Z <$> x
      Right s' -> S <$> go sList s'

-- | Like `cana_NP` but returns a Maybe
mcana_NP ::
  forall c proxy s f xs.
  (All c xs) =>
  proxy c ->
  (forall y ys. (c y, SListI ys) => s (y ': ys) -> Maybe (f y, s ys)) ->
  s xs ->
  Maybe (NP f xs)
mcana_NP _ uncons' = go sList
  where
    go :: forall ys. (All c ys) => SList ys -> s ys -> Maybe (NP f ys)
    go SNil _ = pure Nil
    go SCons s = do
      (x, s') <- uncons' s
      xs <- go sList s'
      pure $ x :* xs
