{-# OPTIONS_GHC -Wno-orphans #-}

{- | Merging multiple Ema sites into one.

    This is implemented in using `sop-core`'s NS and NP types. Use as
    `MultiRoute '[MySite1, MySite2, ...]`.
-}
module Ema.Multi (
  MultiRoute,
) where

import Data.SOP (I (..), NP (..), NS (..))
import Ema.Asset (CanGenerate (..), CanRender (..))
import Ema.Model (HasModel (..))
import Ema.Route.Class (IsRoute (..), here, there)
import Ema.Route.Encoder
import Optics.Core (Iso', equality, iso, prism', review)

{- | The merged site's route is represented as a n-ary sum (`NS`) of the
 sub-routes.
-}
type MultiRoute (rs :: [Type]) = NS I rs

type family MultiModel (rs :: [Type]) :: [Type] where
  MultiModel '[] = '[]
  MultiModel (r ': rs) = RouteModel r : MultiModel rs

type family MultiModelInput (rs :: [Type]) :: [Type] where
  MultiModelInput '[] = '[]
  MultiModelInput (r ': rs) = ModelInput r : MultiModel rs

instance IsRoute (MultiRoute '[]) where
  type RouteModel (MultiRoute '[]) = NP I '[]
  routeEncoder = impossibleEncoder
    where
      impossibleEncoder :: RouteEncoder (NP I '[]) (MultiRoute '[])
      impossibleEncoder = mkRouteEncoder $ \Nil ->
        prism' (\case {}) (const Nothing)

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

instance HasModel (MultiRoute '[]) where
  type ModelInput (MultiRoute '[]) = NP I '[]
  modelDynamic _ _ Nil = pure $ pure Nil

instance
  ( HasModel r
  , HasModel (MultiRoute rs)
  , ModelInput (MultiRoute rs) ~ NP I (MultiModelInput rs)
  , RouteModel (MultiRoute rs) ~ NP I (MultiModel rs)
  ) =>
  HasModel (MultiRoute (r ': rs))
  where
  type ModelInput (MultiRoute (r ': rs)) = NP I (ModelInput r ': MultiModelInput rs)
  modelDynamic cliAct enc (I i :* is) = do
    m <- modelDynamic @r cliAct (headEncoder enc) i
    ms <- modelDynamic @(MultiRoute rs) cliAct (tailEncoder enc) is
    pure $ curry toNP <$> m <*> ms

instance CanRender (MultiRoute '[]) where
  routeAsset _ Nil = \case {}

instance
  ( CanRender r
  , CanRender (MultiRoute rs)
  , RouteModel (MultiRoute rs) ~ NP I (MultiModel rs)
  ) =>
  CanRender (MultiRoute (r ': rs))
  where
  routeAsset enc (I m :* ms) =
    fromNS
      >>> either
        (routeAsset @r (headEncoder enc) m)
        (routeAsset @(MultiRoute rs) (tailEncoder enc) ms)

instance CanGenerate (MultiRoute '[]) where
  generatableRoutes Nil = mempty

instance
  ( CanGenerate r
  , CanGenerate (MultiRoute rs)
  , RouteModel (MultiRoute rs) ~ NP I (MultiModel rs)
  ) =>
  CanGenerate (MultiRoute (r ': rs))
  where
  generatableRoutes (I m :* ms) =
    fmap (toNS . Left) (generatableRoutes @r m)
      <> fmap (toNS . Right) (generatableRoutes @(MultiRoute rs) ms)

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
