{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Class (
  IsRoute (RouteModel, routeEncoder, allRoutes),
  gRouteEncoder,
  SingleModelRoute (..),

  -- * Sub routes
  innerRouteEncoder,
  innerModel,

  -- * Generic helpers
  here,
  there,
) where

import Data.List ((!!))
import Data.SOP.NP (cpure_POP)
import Data.Set qualified as Set
import Ema.Route.Encoder
import Ema.Route.Generic
import GHC.TypeLits (
  ErrorMessage (ShowType, Text, (:$$:)),
  TypeError,
 )
import Generics.SOP
import Optics.Core (
  A_Prism,
  Is,
  Iso',
  NoIx,
  Optic',
  coercedTo,
  equality,
  iso,
  prism',
  review,
  view,
  (%),
 )
import System.FilePath (
  joinPath,
  splitDirectories,
  (</>),
 )
import Prelude hiding (All, Generic)

{- | Class of Ema routes

  An Ema route has an encoder, that knows how to convert it to/from filepaths.
  As well as an universe function, `allRoutes`, that gives all possible route
  values in a static site.

  Both the encoder and the universe function take the associated model as an argument.
-}
class IsRoute r where
  type RouteModel r :: Type
  type RouteModel r = NP I (GRouteModel (Code r))
  routeEncoder :: RouteEncoder (RouteModel r) r
  default routeEncoder ::
    ( Generic r
    , ms ~ GRouteModel (Code r)
    , All2 IsRoute (Code r)
    , All (IsRouteProd ms) (Code r)
    , HasDatatypeInfo r
    , RouteModel r ~ NP I ms
    ) =>
    RouteEncoder (RouteModel r) r
  routeEncoder = gRouteEncoder
  allRoutes :: RouteModel r -> [r]
  -- The default implementation uses generics to compute the enumeration
  -- recursively.
  default allRoutes ::
    ( All2 IsRoute (Code r)
    , All2 (IsRouteIn ms) (Code r)
    , HasDatatypeInfo r
    , ms ~ GRouteModel (Code r)
    , All (IsRouteProd ms) (Code r)
    , RouteModel r ~ NP I ms
    , Ord r
    ) =>
    RouteModel r ->
    [r]
  allRoutes = gallRoutes

gallRoutes ::
  forall r ms.
  ( All2 IsRoute (Code r)
  , All2 (IsRouteIn ms) (Code r)
  , HasDatatypeInfo r
  , ms ~ GRouteModel (Code r)
  , All (IsRouteProd ms) (Code r)
  , Ord r
  ) =>
  NP I ms ->
  [r]
gallRoutes m =
  let pop =
        cpure_POP
          (Proxy @(IsRouteIn ms))
          insideRoutes
      -- FIXME: Can we use traverse due to Applicative instance list.
      pops =
        hcfor
          (Proxy @IsRoute)
          pop
          id
      -- Workaround duplicates routes frmo the FIXME above.
      removeDups = Set.toList . Set.fromList
   in removeDups $ to <$> concatMap apInjs_POP pops
  where
    insideRoutes :: forall b. (IsRouteIn ms b) => [b]
    insideRoutes =
      let m' = view (npIso @_ @(RouteModel b)) m
       in allRoutes m'

{- | DerivingVia repr for routes that use a single model for all inner routes.

 This uses NPConst to support >1 constr with same model.
-}
newtype SingleModelRoute (m :: Type) r = SingleModelRoute {unSingleModelRoute :: r}

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

instance
  ( GRouteModel (Code r) ~ ms
  , NPConst I ms m
  , HasDatatypeInfo r
  , All2 IsRoute (Code r)
  , All2 (IsRouteIn ms) (Code r)
  , All (IsRouteProd ms) (Code r)
  , Ord r
  ) =>
  IsRoute (SingleModelRoute m r)
  where
  type RouteModel (SingleModelRoute m r) = m
  routeEncoder =
    gRouteEncoder @r
      & mapRouteEncoder
        equality
        coercedTo
        (npConstFrom . I)
  allRoutes m =
    SingleModelRoute <$> gallRoutes @r (npConstFrom . I $ m)

instance IsRoute () where
  type RouteModel () = ()
  routeEncoder = singletonRouteEncoder
  allRoutes () = [()]

type family GRouteModel (xss :: [[Type]]) :: [Type] where
  GRouteModel '[] = '[]
  GRouteModel ('[] ': xss) = GRouteModel xss
  GRouteModel ('[x] ': xss) = RouteModel x `UnitCons` GRouteModel xss
-- TODO: reuse from below
  GRouteModel (_ ': _) = TypeError ( 'Text "More than 1 route product")

type family UnitCons x xs where
  UnitCons () xs = xs
  UnitCons (NP I '[]) xs = xs
  UnitCons x xs = x ': xs

-- TODO: Can this be simplified?
class Contains (xs :: [Type]) (x :: Type) where
  -- | A partial iso into/from NP, given a member type. When creating the outer NP structure, rest of the members will be `undefined`.
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

instance {-# OVERLAPPING #-} Contains '[] (NP I '[]) where
  -- NP I '[] is always contained in any structure.
  npIso = iso (const Nil) id

instance {-# OVERLAPPING #-} Contains xs () => Contains (x ': xs) () where
  npIso = there % npIso

instance (TypeError ( 'Text "The type " ':$$: 'ShowType x ':$$: 'Text " does not exist in n-ary product")) => Contains '[] x where
  npIso = iso willNotBeUsed willNotBeUsed

instance {-# OVERLAPPING #-} Contains (x ': xs) x where
  npIso = here

instance {-# OVERLAPPABLE #-} Contains xs x => Contains (x' ': xs) x where
  npIso = there % npIso

{- | Extract the inner RouteEncoder.
 TODO: avoid having to specify Prism
-}
innerRouteEncoder ::
  forall m o i (ms :: [Type]) pf.
  pf `Is` A_Prism =>
  Contains ms m =>
  Optic' pf NoIx o i ->
  RouteEncoder (NP I ms) o ->
  RouteEncoder m i
innerRouteEncoder r =
  mapRouteEncoder equality r (review npIso)

innerModel :: Contains ms m => NP I ms -> m
innerModel = view npIso

-- TODO: Fail in compile time, if ctor naming is bad.
-- TODO: Can I simplify this using `prefixRouteEncoder`?
gRouteEncoder ::
  forall r ms.
  ( Generic r
  , ms ~ GRouteModel (Code r)
  , All2 IsRoute (Code r)
  , All (IsRouteProd ms) (Code r)
  , HasDatatypeInfo r
  ) =>
  RouteEncoder (NP I ms) r
gRouteEncoder =
  mkRouteEncoder $ \m ->
    prism' (gEncodeRoute m) (gDecodeRoute m)

gEncodeRoute ::
  forall r ms.
  ( Generic r
  , ms ~ GRouteModel (Code r)
  , All2 IsRoute (Code r)
  , All (IsRouteProd ms) (Code r)
  , HasDatatypeInfo r
  ) =>
  NP I ms ->
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
      K . encodeRoute (routeEncoder @b) (view (npIso @_ @(RouteModel b)) m) . unI

class (IsRoute r, Contains ms (RouteModel r)) => IsRouteIn ms r

instance (IsRoute r, Contains ms (RouteModel r)) => IsRouteIn ms r

class (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsRouteProd ms xs

instance (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsRouteProd ms xs

gDecodeRoute ::
  forall r ms.
  ( Generic r
  , ms ~ GRouteModel (Code r)
  , All2 IsRoute (Code r)
  , All (IsRouteProd ms) (Code r)
  , HasDatatypeInfo r
  ) =>
  NP I ms ->
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
        -- TODO: replace y1, etc. with sensible vars
        anamorphismProduct :: forall y1 ys1. (IsRouteIn ms y1, SListI ys1) => Proxy (y1 ': ys1) -> Maybe (I y1, Proxy ys1)
        anamorphismProduct Proxy = case sList @ys1 of
          SNil -> do
            -- Recurse into the only product argument
            guard $ not $ null rest
            r' <- decodeRoute (routeEncoder @y1) (view (npIso @_ @(RouteModel y1)) m) $ joinPath rest
            pure (I r', Proxy)
          SCons ->
            -- Not reachable, due to HCollapseMaybe constraint
            Nothing
