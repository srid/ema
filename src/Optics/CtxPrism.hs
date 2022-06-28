module Optics.CtxPrism (
  -- * Type
  CtxPrism,

  -- * Construction
  fromPrism,
  toPrism,

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

 NOTE: This can't actually be a prism itself due to coercion problems. Use
 `toPrism` & `fromPrism`.  See https://stackoverflow.com/q/71489589/55246
-}
type CtxPrism ctx s a =
  -- FIXME: ought to be `ctx -> Prism' s a`. See the NOTE above.
  ctx -> (a -> s, s -> Maybe a)

toPrism :: CtxPrism ctx s a -> ctx -> Prism' s a
toPrism f ctx = let (x, y) = f ctx in prism' x y

fromPrism :: (ctx -> Prism' s a) -> CtxPrism ctx s a
fromPrism f ctx = let p = f ctx in (review p, preview p)

-- | Map a `CtxPrism`
cpmap ::
  forall a b c d x y.
  Prism' b a ->
  Prism' c d ->
  (y -> x) ->
  CtxPrism x a c ->
  CtxPrism y b d
cpmap p q f (toPrism -> r) =
  fromPrism $ \ctx -> p % r (f ctx) % q

{- | Check if the @CtxPrism@ is lawful.

  A @CtxPrism@ is lawful if its conversions both the ways form an isomorphism
  for a given value.

  Returns a Writer reporting logs.
-}
ctxPrismIsLawfulFor ::
  forall ctx s a.
  (Eq a, Eq s, Show a, ToText s) =>
  CtxPrism ctx s a ->
  ctx ->
  a ->
  s ->
  Writer [Text] Bool
ctxPrismIsLawfulFor (toPrism -> cp) ctx =
  prismIsLawfulFor (cp ctx)

prismIsLawfulFor ::
  forall s a.
  (Eq a, Eq s, Show a, ToText s) =>
  Prism' s a ->
  a ->
  s ->
  Writer [Text] Bool
prismIsLawfulFor p a s = do
  -- TODO: The logging here could be improved.
  log $ "Testing Partial ISO law for " <> show a <> " and " <> toText s
  let s' :: s = review p a
  log $ "CtxPrism actual encoding: " <> toText s'
  let ma' :: Maybe a = preview p s'
  log $ "Decoding of that encoding: " <> show ma'
  unless (s == s') $
    log $ "ERR(encoded): " <> toText s <> " /= " <> toText s'
  unless (Just a == ma') $
    log $ "ERR(decoded): " <> show (Just a) <> " /= " <> show ma'
  pure $ (s == s') && (Just a == ma')
  where
    log = tell . one
