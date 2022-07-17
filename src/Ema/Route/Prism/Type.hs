module Ema.Route.Prism.Type where

import Optics.Core (A_Prism, Is, NoIx, Optic', Prism', castOptic, preview, prism', review, (%))

--  DerivingVia prevents us from directly using Prism' here
--  https://stackoverflow.com/q/71489589/55246

{- | Isomorphic to @Prism' s a@, but coercion-friendly.

 Use `fromPrism_` and `toPrism_` to convert between the optics @Prism'@ and this
 @Prism_@.
-}
type Prism_ s a = (a -> s, s -> Maybe a)

-- | Convert a `Prism_` to a `Prism'`.
fromPrism_ :: Prism_ s a -> Prism' s a
fromPrism_ = uncurry prism'

-- | Convert a `Prism'` to a `Prism_`.
toPrism_ :: Prism' s a -> Prism_ s a
toPrism_ = review &&& preview

{- | fmap over the filepath, route and model in a route prism.

  Typically you want to use one of the specific variants below.
-}
mapRoutePrism ::
  (pr `Is` A_Prism, pf `Is` A_Prism) =>
  Optic' pf NoIx FilePath FilePath ->
  Optic' pr NoIx r1 r2 ->
  (b -> a) ->
  (a -> Prism_ FilePath r1) ->
  (b -> Prism_ FilePath r2)
mapRoutePrism fp r m enc =
  toPrism_ . cpmap (castOptic fp) (castOptic r) m (fromPrism_ . enc)
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
