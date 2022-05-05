{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Helpers for generics-sop
module Data.SOP.Extra where

import Data.SOP.Constraint (SListIN)
import Data.Text qualified as T
import GHC.TypeLits (
  ErrorMessage (Text),
  TypeError,
 )
import Generics.SOP
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

datatypeCtors :: forall a. HasDatatypeInfo a => NP ConstructorInfo (Code a)
datatypeCtors = constructorInfo $ datatypeInfo (Proxy @a)

ctorStripPrefix :: forall a. HasDatatypeInfo a => ConstructorName -> String
ctorStripPrefix ctorName =
  let name = datatypeName $ datatypeInfo (Proxy @a)
   in maybe (error $ toText $ "ctor: bad naming: " <> ctorName) (toString . T.toLower) $
        T.stripPrefix (toText $ name <> "_") (toText ctorName)

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
