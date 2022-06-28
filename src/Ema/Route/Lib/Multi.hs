{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Merging multiple Ema sites into one.

    This is implemented in using `sop-core`'s NS and NP types. Use as
    `MultiRoute '[MySite1, MySite2, ...]`.
-}
module Ema.Route.Lib.Multi (
  MultiRoute,
  MultiModel,
) where

import Data.SOP (I (..), NP (..), NS (..))
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder
import Ema.Site (EmaSite (..))
import Optics.Core (equality, iso, prism', Iso', view, review)

{- | The merged site's route is represented as a n-ary sum (`NS`) of the
 sub-routes.
-}
type MultiRoute (rs :: [Type]) = NS I rs

type MultiModel rs = FilterNonUnit (MultiModel' rs)

type MultiSiteArg rs = FilterNonUnit (MultiSiteArg' rs)

instance
  ( IsRoute (MultiRoute' rs)
  , NP I (MultiModel' rs) ~ RouteModel (MultiRoute' rs)
  , FilterNonUnitIso (MultiModel' rs)
  ) =>
  IsRoute (MultiRoute rs)
  where
  type RouteModel (MultiRoute rs) = NP I (MultiModel rs)
  routeEncoder = mapRouteEncoderModel (review filterNonUnitIso) $
                 mapRouteEncoderRoute (iso unMultiRoute' MultiRoute') $
                 routeEncoder @(MultiRoute' rs)
  allRoutes m = coerce <$> allRoutes @(MultiRoute' rs) (review (filterNonUnitIso) m)

instance
  ( EmaSite (MultiRoute' rs)
  , IsRoute (MultiRoute' rs)
  , NP I (MultiModel' rs) ~ RouteModel (MultiRoute' rs)
  , NP I (MultiSiteArg' rs) ~ SiteArg (MultiRoute' rs)
  , FilterNonUnitIso (MultiModel' rs)
  , FilterNonUnitIso (MultiSiteArg' rs)
  ) =>
  EmaSite (MultiRoute rs)
  where
  type SiteArg (MultiRoute rs) = NP I (MultiSiteArg rs)
  siteInput act m = view filterNonUnitIso <<$>>
                    siteInput @(MultiRoute' rs) act (review filterNonUnitIso m)
  siteOutput enc m r = siteOutput @(MultiRoute' rs) enc' m' r'
    where
      enc' = mapRouteEncoderModel (view filterNonUnitIso) $
             mapRouteEncoderRoute (iso MultiRoute' unMultiRoute') enc
      m' = review filterNonUnitIso m
      r' = MultiRoute' r


-- * Auxiliar stuff

-- | Auxiliar type
newtype MultiRoute' (rs :: [Type]) = MultiRoute' { unMultiRoute' :: NS I rs }
  deriving stock (Generic)

type family MultiModel' (rs :: [Type]) :: [Type] where
  MultiModel' '[] = '[]
  MultiModel' (r ': rs) = RouteModel r : MultiModel' rs

type family MultiSiteArg' (rs :: [Type]) :: [Type] where
  MultiSiteArg' '[] = '[]
  MultiSiteArg' (r ': rs) = SiteArg r : MultiSiteArg' rs

instance IsRoute (MultiRoute' '[]) where
  type RouteModel (MultiRoute' '[]) = NP I '[]
  routeEncoder = impossibleEncoder
    where
      impossibleEncoder :: RouteEncoder (NP I '[]) (MultiRoute' '[])
      impossibleEncoder = mkRouteEncoder $ \Nil ->
        prism' (\case {}) (const Nothing)
  allRoutes Nil = mempty

instance
  ( IsRoute r
  , IsRoute (MultiRoute' rs)
  , RouteModel (MultiRoute' rs) ~ NP I (MultiModel' rs)
  ) =>
  IsRoute (MultiRoute' (r ': rs))
  where
  type RouteModel (MultiRoute' (r ': rs)) = NP I (RouteModel r ': MultiModel' rs)
  routeEncoder =
    routeEncoder @r
      `nsRouteEncoder` routeEncoder @(MultiRoute' rs)
  allRoutes (I m :* ms) =
    fmap (toNS . Left) (allRoutes @r m)
      <> fmap (toNS . Right) (allRoutes @(MultiRoute' rs) ms)

instance EmaSite (MultiRoute' '[]) where
  type SiteArg (MultiRoute' '[]) = NP I '[]
  siteInput _ Nil = pure $ pure Nil
  siteOutput _ Nil = \case {}

instance
  ( EmaSite r
  , EmaSite (MultiRoute' rs)
  , SiteArg (MultiRoute' rs) ~ NP I (MultiSiteArg' rs)
  , RouteModel (MultiRoute' rs) ~ NP I (MultiModel' rs)
  ) =>
  EmaSite (MultiRoute' (r ': rs))
  where
  type SiteArg (MultiRoute' (r ': rs)) = NP I (MultiSiteArg' (r ': rs))
  siteInput cliAct (I i :* is) = do
    m <- siteInput @r cliAct i
    ms <- siteInput @(MultiRoute' rs) cliAct is
    pure $ curry toNP <$> m <*> ms
  siteOutput enc (I m :* ms) =
    fromNS
      >>> either
        (siteOutput @r (headEncoder enc) m)
        (siteOutput @(MultiRoute' rs) (tailEncoder enc) ms)

tailEncoder :: RouteEncoder (NP I (MultiModel' (r ': rs))) (MultiRoute' (r ': rs)) -> RouteEncoder (NP I (MultiModel' rs)) (MultiRoute' rs)
tailEncoder =
  mapRouteEncoder equality (prism' (toNS . Right) (fromNS >>> rightToMaybe)) shiftModel
  where
    shiftModel x = I undefined :* x

headEncoder :: RouteEncoder (NP I (MultiModel' (r ': rs))) (MultiRoute' (r ': rs)) -> RouteEncoder (RouteModel r) r
headEncoder =
  mapRouteEncoder equality (prism' (toNS . Left) (fromNS >>> leftToMaybe)) hereModel
  where
    hereModel x = I x :* undefined

-- | Like `eitherRouteEncoder` but uses sop-core types instead of Either/Product.
nsRouteEncoder ::
  RouteEncoder a r ->
  RouteEncoder (NP I as) (MultiRoute' rs) ->
  RouteEncoder (NP I (a ': as)) (MultiRoute' (r ': rs))
nsRouteEncoder a b =
  eitherRouteEncoder a b
    & mapRouteEncoderRoute (iso toNS fromNS)
    & mapRouteEncoderModel fromNP

fromNP :: NP I (a ': as) -> (a, NP I as)
fromNP (I x :* y) = (x, y)

toNP :: (a, NP I as) -> NP I (a ': as)
toNP (x, y) = I x :* y

fromNS :: MultiRoute' (a ': as) -> Either a (MultiRoute' as)
fromNS = \case
  MultiRoute' (Z (I x)) -> Left x
  MultiRoute' (S xs) -> Right (MultiRoute' xs)

toNS :: Either a (MultiRoute' as) -> MultiRoute' (a ': as)
toNS = either (MultiRoute' . Z . I) (MultiRoute' . S . unMultiRoute')

--- Perhaps move this elsewhere

-- | Remove all unit types from a list of types
type family FilterNonUnit (xs :: [Type]) :: [Type] where
  FilterNonUnit '[] = '[]
  FilterNonUnit (() : xs) = FilterNonUnit xs
  FilterNonUnit (x : xs) = x : FilterNonUnit xs

-- | This class witness the isomorphism (xs :: [Type]) ~ (FilterNonUnit xs)
class FilterNonUnitIso (xs :: [Type]) where
  filterNonUnitIso :: Iso' (NP I xs) (NP I (FilterNonUnit xs))

instance FilterNonUnitIso '[] where
  filterNonUnitIso = equality

instance (FilterNonUnitIso xs) => FilterNonUnitIso (() : xs) where
  filterNonUnitIso = iso
                  (\(I () :* xs) -> view filterNonUnitIso xs)
                  (\xs -> I () :* review filterNonUnitIso xs)

instance {-# OVERLAPPABLE #-}
  ( FilterNonUnitIso xs
  , (x : FilterNonUnit xs) ~ FilterNonUnit (x : xs)
  ) =>
  FilterNonUnitIso (x : xs)
  where
  filterNonUnitIso = iso from' to'
    where
      from' :: NP I (x : xs) -> NP I (x : FilterNonUnit xs)
      from' (x :* xs) = x :* view filterNonUnitIso xs
      to' :: NP I (x : FilterNonUnit xs) -> NP I (x : xs)
      to' (x :* xs) = x :* review filterNonUnitIso xs
