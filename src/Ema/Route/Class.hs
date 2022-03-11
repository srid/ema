{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Class
  ( IsRoute (RouteModel, mkRouteEncoder),
    gMkRouteEncoder,
    ConstModelRoute (..),

    -- * Sub routes
    innerRouteEncoder,
    innerModel,
  )
where

import Data.List ((!!))
import Ema.Route.Encoder
import Ema.Route.Generic
import GHC.TypeLits
  ( ErrorMessage (ShowType, Text, (:$$:)),
    TypeError,
  )
import Generics.SOP
import Optics.Core (Prism', iso, prism')
import System.FilePath
  ( joinPath,
    splitDirectories,
    (</>),
  )
import Prelude hiding (All, Generic)

class IsRoute r where
  type RouteModel r :: Type
  type RouteModel r = NPMaybe I (GRouteModel (Code r))
  mkRouteEncoder :: RouteEncoder (RouteModel r) r
  default mkRouteEncoder ::
    ( Generic r,
      ms ~ GRouteModel (Code r),
      All2 IsRoute (Code r),
      All (IsRouteProd ms) (Code r),
      HasDatatypeInfo r,
      RouteModel r ~ NPMaybe I ms,
      UnNPMaybe I ms
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
    gMkRouteEncoder @r & mapRouteEncoder (iso id Just) (prism' unConstModelRoute (Just . ConstModelRoute)) (const ())

instance IsRoute () where
  type RouteModel () = ()
  mkRouteEncoder = singletonRouteEncoder

type family GRouteModel (xss :: [[Type]]) :: [Type] where
  GRouteModel '[] = '[]
  GRouteModel ('[] ': xss) = GRouteModel xss
  GRouteModel ('[x] ': xss) = RouteModel x `UnitCons` GRouteModel xss
-- TODO: reuse from below
  GRouteModel (_ ': _) = TypeError ('Text "More than 1 route product")

type family UnitCons x xs where
  UnitCons () xs = xs
  UnitCons x xs = x ': xs

-- | TODO: This is like lens, but partial?
class HasModel (xs :: [Type]) (x :: Type) where
  -- | Extract the inner model
  innerModel :: NP I xs -> x

  -- | Fill in the outter model containing the given inner model.
  outerModel :: x -> NP I xs

instance {-# OVERLAPPING #-} HasModel '[] () where
  innerModel _ = ()
  outerModel () = Nil

instance {-# OVERLAPPING #-} HasModel xs () => HasModel (x ': xs) () where
  innerModel _ = ()
  outerModel () = I undefined :* outerModel ()

instance (TypeError ('Text "No such model" ':$$: 'ShowType x)) => HasModel '[] x where
  innerModel Nil = undefined
  outerModel _ = undefined

instance {-# OVERLAPPING #-} HasModel (x ': xs) x where
  innerModel (I x :* _) = x
  outerModel x = I x :* undefined

instance {-# OVERLAPPABLE #-} HasModel xs x => HasModel (x' ': xs) x where
  innerModel (_ :* xs) = innerModel xs
  outerModel x = I undefined :* outerModel x

-- TODO: avoid Iso using https://hackage.haskell.org/package/generic-lens

-- | Extract the inner RouteEncoder.
innerRouteEncoder ::
  forall m o i (ms :: [Type]).
  HasModel ms m =>
  Prism' o i ->
  -- Iso o o (Maybe i) i ->
  RouteEncoder (NP I ms) o ->
  RouteEncoder m i
innerRouteEncoder prism =
  mapRouteEncoder (iso id Just) prism outerModel

-- TODO: Fail in compile time, if ctor naming is bad.
gMkRouteEncoder ::
  forall r ms.
  ( Generic r,
    ms ~ GRouteModel (Code r),
    All2 IsRoute (Code r),
    All (IsRouteProd ms) (Code r),
    HasDatatypeInfo r,
    UnNPMaybe I ms
  ) =>
  RouteEncoder (NPMaybe I ms) r
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
    HasDatatypeInfo r,
    UnNPMaybe I ms
  ) =>
  NPMaybe I ms ->
  r ->
  FilePath
gEncodeRoute (unNPMaybe @_ @_ @ms -> m) x' =
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
      K . encodeRoute (mkRouteEncoder @b) (innerModel @_ @(RouteModel b) m) . unI

class (IsRoute r, HasModel ms (RouteModel r), UnNPMaybe I ms) => IsRouteIn ms r

instance (IsRoute r, HasModel ms (RouteModel r), UnNPMaybe I ms) => IsRouteIn ms r

class (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsRouteProd ms xs

instance (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsRouteProd ms xs

gDecodeRoute ::
  forall r ms.
  ( Generic r,
    ms ~ GRouteModel (Code r),
    All2 IsRoute (Code r),
    All (IsRouteProd ms) (Code r),
    UnNPMaybe I ms,
    HasDatatypeInfo r
  ) =>
  NPMaybe I ms ->
  FilePath ->
  Maybe r
gDecodeRoute (unNPMaybe @_ @I @ms -> m) fp = do
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
            r' <- decodeRoute (mkRouteEncoder @y1) (innerModel @_ @(RouteModel y1) m) $ joinPath rest
            pure (I r', Proxy)
          SCons ->
            -- Not reachable, due to HCollapseMaybe constraint
            Nothing
