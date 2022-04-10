{-# OPTIONS_GHC -Wno-orphans #-}

{- | Merging multiple Ema sites into one.

    This is implemented in using `sop-core`'s NS and NP types.
-}
module Ema.Multi where

import Data.SOP
import Ema.Asset
import Ema.Model
import Ema.Route.Class
import Ema.Route.Encoder
import Optics.Core (coercedTo, equality, prism', review, (%))

{- | The merged site's route is represented as a n-ary sum (`NS`) of the
 sub-routes.
-}
type MultiRoute (rs :: [Type]) = NS I rs

type family MultiModel (rs :: [Type]) :: [Type] where
  MultiModel '[] = '[]
  MultiModel (r ': rs) = RouteModel r : MultiModel rs

instance IsRoute (MultiRoute rs) where
  type RouteModel (MultiRoute rs) = NP I (MultiModel rs)
  routeEncoder = undefined

instance HasModel (MultiRoute rs) where
  type ModelInput (MultiRoute rs) = ()
  modelDynamic _ _ () = do
    undefined

instance CanRender (MultiRoute '[]) where
  routeAsset _ Nil = \case {}

instance (CanRender r, CanRender (MultiRoute rs)) => CanRender (MultiRoute (r ': rs)) where
  routeAsset enc (I m :* ms) = \case
    Z (I r) ->
      routeAsset @r (headEncoder enc) m r
    S rs' ->
      routeAsset @(MultiRoute rs) (tailEncoder enc) ms rs'

tailEncoder :: RouteEncoder (NP I (MultiModel (r ': rs))) (MultiRoute (r ': rs)) -> RouteEncoder (NP I (MultiModel rs)) (MultiRoute rs)
tailEncoder =
  mapRouteEncoder equality (prism' S f) (review there)
  where
    f :: NS I (r ': rs) -> Maybe (NS I rs)
    f = \case
      Z _ -> Nothing
      S rs' -> Just rs'

headEncoder :: RouteEncoder (NP I (MultiModel (r ': rs))) (MultiRoute (r ': rs)) -> RouteEncoder (RouteModel r) r
headEncoder =
  mapRouteEncoder equality (prism' (Z . I) f) (review here)
  where
    f :: NS I (r ': rs) -> Maybe r
    f = \case
      Z (I r) -> Just r
      _ -> Nothing

instance CanGenerate (MultiRoute '[]) where
  generatableRoutes Nil = mempty

instance (CanGenerate r, CanGenerate (MultiRoute rs)) => CanGenerate (MultiRoute (r ': rs)) where
  generatableRoutes (I m :* ms) =
    fmap (Z . I) (generatableRoutes @r m)
      <> fmap S (generatableRoutes @(MultiRoute rs) ms)