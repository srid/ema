{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Class (
  IsRoute (RouteModel, routeEncoder, allRoutes),
  SingleModelRoute (..),
  gRouteEncoder,
  gAllRoutes,
) where

import Data.List ((!!))
import Data.SOP.Extra
import Data.SOP.NP (cpure_POP)
import Ema.Route.Encoder
import GHC.TypeLits (
  ErrorMessage (Text),
  TypeError,
 )
import Generics.SOP
import Optics.Core
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
    , All (IsSubRouteIn ms) (Code r)
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
    , All (IsSubRouteIn ms) (Code r)
    , RouteModel r ~ NP I ms
    , Ord r
    ) =>
    RouteModel r ->
    [r]
  allRoutes = gAllRoutes

{- | DerivingVia repr for routes that use a single model for all inner routes.

 This uses NPConst to support >1 constr with same model.

 NOTE: Perhaps we want to replace this with deriving-aeson like `CustomJSON`
 type, for customizing other things (like striping prefixes).
-}
newtype SingleModelRoute (m :: Type) r = SingleModelRoute {unSingleModelRoute :: r}

instance
  ( GRouteModel (Code r) ~ ms
  , NPConst I ms m
  , HasDatatypeInfo r
  , All2 IsRoute (Code r)
  , All2 (IsRouteIn ms) (Code r)
  , All (IsSubRouteIn ms) (Code r)
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
    SingleModelRoute
      <$> gAllRoutes @r (npConstFrom . I $ m)

{- | Generic `RouteModel`

  Produces a heterogenous list of models that are pulled from each constructors.
-}
type family GRouteModel (xss :: [[Type]]) :: [Type] where
  GRouteModel '[] = '[]
  GRouteModel ('[] ': xss) = GRouteModel xss
  GRouteModel ('[x] ': xss) = RouteModel x `ModelCons` GRouteModel xss
-- TODO: reuse from below
  GRouteModel (_ ': _) = TypeError ( 'Text "More than 1 route product")

-- | Does a cons on models, ignoring 'unit' models.
type family ModelCons x xs where
  ModelCons () xs = xs
  ModelCons (NP I '[]) xs = xs
  ModelCons x xs = x ': xs

-- TODO: Can I simplify this using `prefixRouteEncoder`?
gRouteEncoder ::
  forall r ms.
  ( Generic r
  , ms ~ GRouteModel (Code r)
  , All2 IsRoute (Code r)
  , All (IsSubRouteIn ms) (Code r)
  , HasDatatypeInfo r
  ) =>
  RouteEncoder (NP I ms) r
gRouteEncoder =
  mkRouteEncoder $ \m ->
    prism' (gEncodeRoute m) (gDecodeRoute m)

-- | Like `IsRoute` but with `ms` being the superset of `RouteModel r`
class (IsRoute r, NPContains ms (RouteModel r)) => IsRouteIn ms r

instance (IsRoute r, NPContains ms (RouteModel r)) => IsRouteIn ms r

-- | Class of constructors (with fields `xs`) that represent sub-routes.
class (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsSubRouteIn ms xs

instance (All (IsRouteIn ms) xs, HCollapseMaybe NP xs) => IsSubRouteIn ms xs

gEncodeRoute ::
  forall r ms.
  ( Generic r
  , ms ~ GRouteModel (Code r)
  , All2 IsRoute (Code r)
  , All (IsSubRouteIn ms) (Code r)
  , HasDatatypeInfo r
  ) =>
  NP I ms ->
  r ->
  FilePath
gEncodeRoute m r' =
  let r = unSOP $ from @r r'
      ctorNames :: [ConstructorName] =
        hcollapse $ hmap (K . constructorName) $ datatypeCtors @r
      ctorSuffix = ctorStripPrefixMust @r $ ctorNames !! hindex r
   in case hcollapse $ hcmap (Proxy @(IsSubRouteIn ms)) encProd r of
        Nothing -> ctorSuffix <> ".html"
        Just p -> ctorSuffix </> p
  where
    encProd :: forall xs. (IsSubRouteIn ms xs) => NP I xs -> K (Maybe FilePath) xs
    encProd =
      K . hcollapseMaybe . hcmap (Proxy @(IsRouteIn ms)) encTerm
    encTerm :: forall b. (IsRouteIn ms b) => I b -> K FilePath b
    encTerm =
      K . encodeRoute (routeEncoder @b) (view (npIso @_ @(RouteModel b)) m) . unI

gDecodeRoute ::
  forall r ms.
  ( Generic r
  , ms ~ GRouteModel (Code r)
  , All2 IsRoute (Code r)
  , All (IsSubRouteIn ms) (Code r)
  , HasDatatypeInfo r
  ) =>
  NP I ms ->
  FilePath ->
  Maybe r
gDecodeRoute m fp = do
  basePath : restPath <- pure $ splitDirectories fp
  -- Build the sum using an anamorphism
  Generics.SOP.to . SOP
    <$> mcana_NS @(IsSubRouteIn ms) @_ @_ @(NP I)
      Proxy
      (anamorphismSum basePath restPath)
      (datatypeCtors @r)
  where
    anamorphismSum :: forall xs xss. IsSubRouteIn ms xs => FilePath -> [FilePath] -> NP ConstructorInfo (xs ': xss) -> Either (Maybe (NP I xs)) (NP ConstructorInfo xss)
    anamorphismSum base rest (p :* ps) =
      fromMaybe (Right ps) $ do
        let ctorSuffix = ctorStripPrefixMust @r (constructorName p)
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

-- TODO: Instead of falling back to original ctorName, raise a compile
-- error on Nothing.
ctorStripPrefixMust :: forall r. HasDatatypeInfo r => ConstructorName -> String
ctorStripPrefixMust ctorName = fromMaybe ctorName $ ctorStripPrefix @r ctorName

gAllRoutes ::
  forall r ms.
  ( All2 IsRoute (Code r)
  , All2 (IsRouteIn ms) (Code r)
  , HasDatatypeInfo r
  , ms ~ GRouteModel (Code r)
  , All (IsSubRouteIn ms) (Code r)
  , Ord r
  ) =>
  NP I ms ->
  [r]
gAllRoutes m =
  let pop =
        cpure_POP
          (Proxy @(IsRouteIn ms))
          insideRoutes
      -- FIXME: Can we avoid duplicates?
      pops =
        hcfor
          (Proxy @IsRoute)
          pop
          id
      -- Workaround duplicates from the FIXME above.
      removeDups = sortNub
   in removeDups $ Generics.SOP.to <$> concatMap apInjs_POP pops
  where
    insideRoutes :: forall b. (IsRouteIn ms b) => [b]
    insideRoutes =
      let m' = view (npIso @_ @(RouteModel b)) m
       in allRoutes m'
