module Ema.Route.Encoder.Check (
  prismIsLawfulFor,
) where

import Control.Monad.Writer (Writer, tell)
import Optics.Core (Prism', preview, review)

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
