module Optics.CtxPrism (
  -- * Type
  CtxPrism,

  -- * Construction
  fromPrism,

  -- * Conversion
  cpreview,
  creview,

  -- * Functor
  cpmap,

  -- * Law checks
  ctxPrismIsLawfulFor,
) where

import Control.Monad.Writer (Writer, tell)
import Optics.Core (
  Prism',
  preview,
  prism',
  review,
  (%),
 )

{- | A `Prism` with a context
 This can't actually be a prism due to coercion problems. Use `toPrism` & `fromPrism`.
 See https://stackoverflow.com/q/71489589/55246
-}
type CtxPrism ctx s a =
  -- FIXME: ought to be `ctx -> Prism' s a`
  ctx -> (a -> s, s -> Maybe a)

toPrism :: CtxPrism ctx s a -> ctx -> Prism' s a
toPrism f' ctx = let (x, y) = f' ctx in prism' x y

fromPrism :: (ctx -> Prism' s a) -> CtxPrism ctx s a
fromPrism f ctx = let p = f ctx in (review p, preview p)

creview :: CtxPrism ctx t b -> ctx -> b -> t
creview p ctx = review (toPrism p ctx)

cpreview :: CtxPrism ctx s a -> ctx -> s -> Maybe a
cpreview p ctx = preview (toPrism p ctx)

ctxPrismIsLawfulFor :: forall ctx s a. (Eq a, Eq s, Show a, ToText s) => CtxPrism ctx s a -> ctx -> a -> s -> Writer [Text] Bool
ctxPrismIsLawfulFor (toPrism -> p') ctx a s = do
  let p = p' ctx
  tell . one $ "Testing Partial ISO law for " <> show a <> " and " <> toText s
  let s' :: s = review p a
  tell . one $ "Route's actual encoding: " <> toText s'
  let ma' :: Maybe a = preview p s'
  tell . one $ "Decoding of that encoding: " <> show ma'
  unless (s == s') $
    tell . one $ "ERR: " <> toText s <> " /= " <> toText s'
  unless (Just a == ma') $
    tell . one $ "ERR: " <> show (Just a) <> " /= " <> show ma'
  pure $ (s == s') && (Just a == ma')

-- | Map a `CtxPrism`
cpmap ::
  forall a b c d x y.
  Prism' b a ->
  Prism' c d ->
  (y -> x) ->
  CtxPrism x a c ->
  CtxPrism y b d
cpmap p q f (toPrism -> r) = fromPrism $ \ctx -> p % r (f ctx) % q
