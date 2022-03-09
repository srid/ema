{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Generic
  ( gMkRouteEncoder,
    IsRoute (RouteModel, mkRouteEncoder),
    ConstModelRoute (..),
    pullOutRouteEncoder,
    getModel,
  )
where

import Control.Lens.Combinators (Iso, Iso', Profunctor (lmap), iso)
import Data.List ((!!))
import Data.SOP.Constraint (SListIN)
import Data.Text qualified as T
import Ema.Route.Encoder
import GHC.TypeLits
import Generics.SOP
import System.FilePath
  ( joinPath,
    splitDirectories,
    (</>),
  )
import Prelude hiding (All, Generic)

class IsRoute r where
  type RouteModel r :: Type
  type RouteModel r = NP I (GRouteModel (Code r))
  mkRouteEncoder :: RouteEncoder (RouteModel r) r
  default mkRouteEncoder ::
    ( Generic r,
      ms ~ GRouteModel (Code r),
      All2 IsRoute (Code r),
      All (IsRouteProd ms) (Code r),
      HasDatatypeInfo r,
      RouteModel r ~ NP I ms
    ) =>
    RouteEncoder (RouteModel r) r
  mkRouteEncoder = gMkRouteEncoder

newtype ConstModelRoute (m :: Type) r = ConstModelRoute {unConstModelRoute :: r}

instance
  ( GRouteModel (Code r) ~ '[],
    HasDatatypeInfo r,
    All2 IsRoute (Code r),
    All (IsRouteProd '[]) (Code r)
  ) =>
  IsRoute (ConstModelRoute m r)
  where
  type RouteModel (ConstModelRoute m r) = m
  mkRouteEncoder =
    gMkRouteEncoder @r & mapRouteEncoder (iso id Just) (iso (Just . ConstModelRoute) unConstModelRoute) (const Nil)

instance IsRoute () where
  type RouteModel () = ()
  mkRouteEncoder = singletonRouteEncoder

type family GRouteModel (xss :: [[Type]]) :: [Type] where
  GRouteModel '[] = '[]
  GRouteModel ('[] ': xss) = GRouteModel xss
  GRouteModel ('[x] ': xss) = RouteModel x ': GRouteModel xss
-- TODO: reuse from below
  GRouteModel (_ ': _) = TypeError ('Text "More than 1 route product")

-- | TODO: This is like lens, but partial?
class HasModel (xs :: [Type]) (x :: Type) where
  getModel :: NP I xs -> x
  putModel :: x -> NP I xs

instance (TypeError ('Text "No such model" ':$$: 'ShowType x)) => HasModel '[] x where
  getModel Nil = undefined
  putModel _ = undefined

instance {-# OVERLAPPING #-} HasModel (x ': xs) x where
  getModel (I x :* _) = x
  putModel x = I x :* undefined

instance {-# OVERLAPPABLE #-} HasModel xs x => HasModel (x' ': xs) x where
  getModel (_ :* xs) = getModel xs
  putModel x = I undefined :* putModel x

pullOutRouteEncoder :: forall m o i (ms :: [Type]). HasModel ms m => Iso o o (Maybe i) i -> RouteEncoder (NP I ms) o -> RouteEncoder m i
pullOutRouteEncoder rIso =
  mapRouteEncoder (iso id Just) rIso putModel

-- TODO: Fail in compile time, if ctor naming is bad.
gMkRouteEncoder ::
  forall r ms.
  ( Generic r,
    ms ~ GRouteModel (Code r),
    All2 IsRoute (Code r),
    All (IsRouteProd ms) (Code r),
    HasDatatypeInfo r
  ) =>
  RouteEncoder (NP I ms) r
gMkRouteEncoder =
  unsafeMkRouteEncoder gEncodeRoute gDecodeRoute (const all_)
  where
    all_ :: [r]
    all_ = [] -- TODO

gEncodeRoute ::
  forall r ms.
  ( Generic r,
    ms ~ GRouteModel (Code r),
    All2 IsRoute (Code r),
    All (IsRouteProd ms) (Code r),
    HasDatatypeInfo r
  ) =>
  NP I (GRouteModel (Code r)) ->
  r ->
  FilePath
gEncodeRoute m x' =
  let x = unSOP $ from @r x'
      ctorNames :: [ConstructorName] =
        hcollapse $ hmap (K . constructorName) $ datatypeCtors @r
      ctorSuffix = ctorStripPrefix @r (ctorNames !! hindex x)
   in case hcollapse $ hcmap (Proxy @(IsRouteProd ms)) encProd x of
        Nothing -> ctorSuffix <> ".html"
        Just p -> ctorSuffix </> p
  where
    encProd :: forall xs. (IsRouteProd ms xs) => NP I xs -> K (Maybe FilePath) xs
    encProd =
      K . hcollapseMaybe . hcmap (Proxy @(IsRouteIn ms)) encTerm
    encTerm :: forall b. (IsRouteIn ms b) => I b -> K FilePath b
    encTerm =
      K . encodeRoute (mkRouteEncoder @b) (getModel @_ @(RouteModel b) m) . unI

-- | Like `HCollapse`, but limited to 0 or 1 products in a n-ary structure.
class HCollapseMaybe h xs where
  hcollapseMaybe :: SListIN h xs => h (K a) xs -> Maybe a

instance HCollapseMaybe NP '[] where
  hcollapseMaybe Nil = Nothing

instance HCollapseMaybe NP '[p] where
  hcollapseMaybe (K x :* Nil) = Just x

instance (ps ~ TypeError ('Text "Expected at most 1 product")) => HCollapseMaybe NP (p ': p1 ': ps) where
  hcollapseMaybe _ = Nothing -- Unreachable, due to TypeError

class (IsRoute r, HasModel ms (RouteModel r)) => IsRouteIn ms r

instance (IsRoute r, HasModel ms (RouteModel r)) => IsRouteIn ms r

class (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsRouteProd ms xs

instance (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsRouteProd ms xs

datatypeCtors :: forall a. HasDatatypeInfo a => NP ConstructorInfo (Code a)
datatypeCtors = constructorInfo $ datatypeInfo (Proxy @a)

ctorStripPrefix :: forall a. HasDatatypeInfo a => ConstructorName -> String
ctorStripPrefix ctorName =
  let name = datatypeName $ datatypeInfo (Proxy @a)
   in maybe (error $ toText $ "ctor: bad naming: " <> ctorName) (T.unpack . T.toLower) $
        T.stripPrefix (T.pack $ name <> "_") (T.pack ctorName)

gDecodeRoute ::
  forall r ms.
  ( Generic r,
    ms ~ GRouteModel (Code r),
    All2 IsRoute (Code r),
    All (IsRouteProd ms) (Code r),
    HasDatatypeInfo r
  ) =>
  NP I (GRouteModel (Code r)) ->
  FilePath ->
  Maybe r
gDecodeRoute m fp = do
  basePath : restPath <- pure $ splitDirectories fp
  -- Build the sum using an anamorphism
  to . SOP
    <$> mcana_NS @(IsRouteProd ms) @_ @_ @(NP I)
      Proxy
      (anamorphismSum basePath restPath)
      (datatypeCtors @r)
  where
    anamorphismSum :: forall xs xss. IsRouteProd ms xs => FilePath -> [FilePath] -> NP ConstructorInfo (xs ': xss) -> Either (Maybe (NP I xs)) (NP ConstructorInfo xss)
    anamorphismSum base rest (p :* ps) =
      fromMaybe (Right ps) $ do
        let ctorSuffix = ctorStripPrefix @r (constructorName p)
        Left <$> case sList @xs of
          SNil -> do
            -- Constructor without arguments
            guard $ ctorSuffix <> ".html" == base && null rest
            pure $ Just Nil
          SCons -> do
            -- Constructor with an argument
            guard $ ctorSuffix == base
            pure $
              mcana_NP @_ @_ @_ @I
                (Proxy @(IsRouteIn ms))
                anamorphismProduct
                Proxy
      where
        anamorphismProduct :: forall y1 ys1. (IsRouteIn ms y1, SListI ys1) => Proxy (y1 ': ys1) -> Maybe (I y1, Proxy ys1)
        anamorphismProduct Proxy = case sList @ys1 of
          SNil -> do
            -- Recurse into the only product argument
            guard $ not $ null rest
            r' <- decodeRoute (mkRouteEncoder @y1) (getModel @_ @(RouteModel y1) m) $ joinPath rest
            pure (I r', Proxy)
          SCons ->
            -- Not reachable, due to HCollapseMaybe constraint
            Nothing

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
mcana_NP _ uncons = go sList
  where
    go :: forall ys. (All c ys) => SList ys -> s ys -> Maybe (NP f ys)
    go SNil _ = pure Nil
    go SCons s = do
      (x, s') <- uncons s
      xs <- go sList s'
      pure $ x :* xs
