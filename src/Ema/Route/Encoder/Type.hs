module Ema.Route.Encoder.Type where

import Optics.Core (A_Prism, Is, NoIx, Optic', Prism', castOptic, preview, prism', review, (%))

{- | An encoder cum decoder that knows how to convert routes to and from
 filepaths. The conversion depends on the context `a`.
-}
type RouteEncoder a r = a -> Prism_ FilePath r

{- | Isomorphic to `Prism' s a`, but coercion-friendly.

  DerivingVia prevents us from directly using Prism' here
  https://stackoverflow.com/q/71489589/55246
-}
type Prism_ s a = (a -> s, s -> Maybe a)

fromPrism_ :: Prism_ s a -> Prism' s a
fromPrism_ = uncurry prism'

toPrism_ :: Prism' s a -> Prism_ s a
toPrism_ = review &&& preview

-- | Make a `RouteEncoder` manually.
mkRouteEncoder :: (a -> Prism' FilePath r) -> RouteEncoder a r
mkRouteEncoder f = toPrism_ . f

applyRouteEncoder :: RouteEncoder a r -> a -> Prism' FilePath r
applyRouteEncoder f = fromPrism_ . f

{- | fmap over the filepath, route and model in a `RouteEncoder`

  Typically you want to use one of the specific variants below.
-}
mapRouteEncoder ::
  (pr `Is` A_Prism, pf `Is` A_Prism) =>
  Optic' pf NoIx FilePath FilePath ->
  Optic' pr NoIx r1 r2 ->
  (b -> a) ->
  RouteEncoder a r1 ->
  RouteEncoder b r2
mapRouteEncoder fp r m enc =
  mkRouteEncoder $ cpmap (castOptic fp) (castOptic r) m $ applyRouteEncoder enc
  where
    cpmap ::
      forall a b c d x y.
      Prism' b a ->
      Prism' c d ->
      (y -> x) ->
      (x -> Prism' a c) ->
      (y -> Prism' b d)
    cpmap p q f r' ctx =
      p % r' (f ctx) % q
