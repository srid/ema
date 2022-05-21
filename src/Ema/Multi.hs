{-# OPTIONS_GHC -Wno-orphans #-}

{- | Merging multiple Ema sites into one.

    This is implemented in using `sop-core`'s NS and NP types. Use as
    `MultiRoute '[MySite1, MySite2, ...]`.
-}
module Ema.Multi (
  MultiRoute,
) where

import Data.SOP (I (..), NP (..), NS (..))
import Ema.Route.Class (IsRoute (..), here, there)
import Ema.Route.Encoder
import Ema.Site (EmaSite (..))
import Optics.Core (equality, iso, prism', review)

{- | The merged site's route is represented as a n-ary sum (`NS`) of the
 sub-routes.
-}
type MultiRoute (rs :: [Type]) = NS I rs

type family MultiModel (rs :: [Type]) :: [Type] where
  MultiModel '[] = '[]
  MultiModel (r ': rs) = RouteModel r : MultiModel rs

type family MultiSiteArg (rs :: [Type]) :: [Type] where
  MultiSiteArg '[] = '[]
  MultiSiteArg (r ': rs) = SiteArg r : MultiModel rs

instance IsRoute (MultiRoute '[]) where
  type RouteModel (MultiRoute '[]) = NP I '[]
  routeEncoder = impossibleEncoder
    where
      impossibleEncoder :: RouteEncoder (NP I '[]) (MultiRoute '[])
      impossibleEncoder = mkRouteEncoder $ \Nil ->
        prism' (\case {}) (const Nothing)
  allRoutes Nil = mempty

instance
  ( IsRoute r
  , IsRoute (MultiRoute rs)
  , RouteModel (MultiRoute rs) ~ NP I (MultiModel rs)
  ) =>
  IsRoute (MultiRoute (r ': rs))
  where
  type RouteModel (MultiRoute (r ': rs)) = NP I (RouteModel r ': MultiModel rs)
  routeEncoder =
    routeEncoder @r
      `hMergeRouteEncoder` routeEncoder @(MultiRoute rs)
  allRoutes (I m :* ms) =
    fmap (toNS . Left) (allRoutes @r m)
      <> fmap (toNS . Right) (allRoutes @(MultiRoute rs) ms)

instance EmaSite (MultiRoute '[]) where
  type SiteArg (MultiRoute '[]) = NP I '[]
  siteInput _ _ Nil = pure $ pure Nil
  siteOutput _ Nil = \case {}

instance
  ( EmaSite r
  , EmaSite (MultiRoute rs)
  , SiteArg (MultiRoute rs) ~ NP I (MultiSiteArg rs)
  , RouteModel (MultiRoute rs) ~ NP I (MultiModel rs)
  ) =>
  EmaSite (MultiRoute (r ': rs))
  where
  type SiteArg (MultiRoute (r ': rs)) = NP I (SiteArg r ': MultiSiteArg rs)
  siteInput cliAct enc (I i :* is) = do
    m <- siteInput @r cliAct (headEncoder enc) i
    ms <- siteInput @(MultiRoute rs) cliAct (tailEncoder enc) is
    pure $ curry toNP <$> m <*> ms
  siteOutput enc (I m :* ms) =
    fromNS
      >>> either
        (siteOutput @r (headEncoder enc) m)
        (siteOutput @(MultiRoute rs) (tailEncoder enc) ms)

tailEncoder :: RouteEncoder (NP I (MultiModel (r ': rs))) (MultiRoute (r ': rs)) -> RouteEncoder (NP I (MultiModel rs)) (MultiRoute rs)
tailEncoder =
  mapRouteEncoder equality (prism' (toNS . Right) (fromNS >>> rightToMaybe)) (review there)

headEncoder :: RouteEncoder (NP I (MultiModel (r ': rs))) (MultiRoute (r ': rs)) -> RouteEncoder (RouteModel r) r
headEncoder =
  mapRouteEncoder equality (prism' (toNS . Left) (fromNS >>> leftToMaybe)) (review here)

-- | Like `mergRouteEncoder` but uses sop-core types instead of Either/Product.
hMergeRouteEncoder ::
  RouteEncoder a r ->
  RouteEncoder (NP I as) (NS I rs) ->
  RouteEncoder (NP I (a ': as)) (NS I (r ': rs))
hMergeRouteEncoder a b =
  mergeRouteEncoder a b
    & mapRouteEncoderRoute (iso toNS fromNS)
    & mapRouteEncoderModel fromNP

fromNP :: NP I (a ': as) -> (a, NP I as)
fromNP (I x :* y) = (x, y)

toNP :: (a, NP I as) -> NP I (a ': as)
toNP (x, y) = I x :* y

fromNS :: NS I (a ': as) -> Either a (NS I as)
fromNS = \case
  Z (I x) -> Left x
  S xs -> Right xs

toNS :: Either a (NS I as) -> NS I (a ': as)
toNS = either (Z . I) S
