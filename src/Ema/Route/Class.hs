{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Class
  ( IsRoute (RouteModel, mkRouteEncoder),
    gMkRouteEncoder,
    ConstModelRoute (..),
    ShowReadable (ShowReadable),
    Stringable (Stringable),

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
import Optics.Core
  ( Iso',
    Prism',
    iso,
    prism',
    review,
    view,
    (%),
  )
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

-- FIXME: this is useless for recursive routes
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
    gMkRouteEncoder @r & mapRouteEncoder (prism' id Just) (prism' unConstModelRoute (Just . ConstModelRoute)) (const ())

newtype ShowReadable a = ShowReadable a
  deriving newtype (Show, Read)

newtype Stringable a = Stringable a
  deriving newtype (ToString, IsString)

instance (Show a, Read a) => IsRoute (ShowReadable a) where
  type RouteModel (ShowReadable a) = ()
  mkRouteEncoder = showReadRouteEncoder

instance (IsString a, ToString a) => IsRoute (Stringable a) where
  type RouteModel (Stringable a) = ()
  mkRouteEncoder = stringRouteEncoder

deriving via (Stringable Text) instance IsRoute Text

deriving via (Stringable String) instance IsRoute String

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

-- | TODO: Can this be simplified?
class (xs :: [Type]) `Contains` (x :: Type) where
  -- | A partial iso into/from NP, given a member type.
  --
  -- When creating the outer NP structure, rest of the members will be
  -- undefined.
  npIso :: Iso' (NP I xs) x

there :: Iso' (NP I (x ': xs)) (NP I xs)
there = iso tl (\t -> I willNotBeUsed :* t)

here :: Iso' (NP I (x ': xs)) x
here = iso (unI . hd) (\x -> I x :* willNotBeUsed)

-- Could probably replace this lens-sop:
-- with https://hackage.haskell.org/package/lens-sop-0.2.0.3/docs/Generics-SOP-Lens.html#v:np
instance {-# OVERLAPPING #-} Contains '[] () where
  -- () is always contained in any structure.
  npIso = iso (const ()) (\() -> Nil)

instance {-# OVERLAPPING #-} Contains xs () => Contains (x ': xs) () where
  npIso = there % npIso

instance (TypeError ('Text "The type " ':$$: 'ShowType x ':$$: 'Text " does not exist in n-ary product")) => Contains '[] x where
  npIso = iso willNotBeUsed willNotBeUsed

instance {-# OVERLAPPING #-} Contains (x ': xs) x where
  npIso = here

instance {-# OVERLAPPABLE #-} Contains xs x => Contains (x' ': xs) x where
  npIso = there % npIso

-- | Extract the inner RouteEncoder.
-- TODO: avoid having to specify Prism
innerRouteEncoder ::
  forall m o i (ms :: [Type]).
  Contains ms m =>
  Prism' o i ->
  RouteEncoder (NP I ms) o ->
  RouteEncoder m i
innerRouteEncoder p =
  mapRouteEncoder (prism' id Just) p (review npIso)

innerModel :: Contains ms m => NP I ms -> m
innerModel = view npIso

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
  unsafeMkRouteEncoder gEncodeRoute gDecodeRoute

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
      K . encodeRoute (mkRouteEncoder @b) (view (npIso @_ @(RouteModel b)) m) . unI

class (IsRoute r, Contains ms (RouteModel r), UnNPMaybe I ms) => IsRouteIn ms r

instance (IsRoute r, Contains ms (RouteModel r), UnNPMaybe I ms) => IsRouteIn ms r

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
        -- TODO: replace y1, etc. with sensible vars
        anamorphismProduct :: forall y1 ys1. (IsRouteIn ms y1, SListI ys1) => Proxy (y1 ': ys1) -> Maybe (I y1, Proxy ys1)
        anamorphismProduct Proxy = case sList @ys1 of
          SNil -> do
            -- Recurse into the only product argument
            guard $ not $ null rest
            r' <- decodeRoute (mkRouteEncoder @y1) (view (npIso @_ @(RouteModel y1)) m) $ joinPath rest
            pure (I r', Proxy)
          SCons ->
            -- Not reachable, due to HCollapseMaybe constraint
            Nothing
