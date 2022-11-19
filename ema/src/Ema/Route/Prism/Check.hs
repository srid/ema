module Ema.Route.Prism.Check (
  checkRoutePrismGivenFilePath,
  checkRoutePrismGivenRoute,
) where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Text qualified as T
import Ema.Route.Prism.Type (Prism_, fromPrism_)
import Optics.Core (Prism', preview, review)
import System.FilePath ((</>))

checkRoutePrismGivenRoute ::
  (HasCallStack, Eq r, Show r) =>
  (a -> Prism_ FilePath r) ->
  a ->
  r ->
  Either Text ()
checkRoutePrismGivenRoute enc a r =
  let s = review (fromPrism_ $ enc a) r
   in checkRoutePrism enc a r s

checkRoutePrismGivenFilePath ::
  (HasCallStack, Eq r, Show r) =>
  (a -> Prism_ FilePath r) ->
  a ->
  FilePath ->
  Either (r, [(FilePath, Text)]) (Maybe r)
checkRoutePrismGivenFilePath enc a s = do
  -- We should treat /foo, /foo.html and /foo/index.html as equivalent.
  let candidates = [s, s <> ".html", s </> "index.html"]
      rp = fromPrism_ $ enc a
  case asum (preview rp <$> candidates) of
    Nothing -> pure Nothing
    Just r -> do
      -- All candidates must be checked, and if even one passes - we let this
      -- route go through.
      let (failed, passed) =
            partitionEithers $
              candidates <&> \candidate ->
                case checkRoutePrism enc a r candidate of
                  Left err -> Left (candidate, err)
                  Right () -> Right ()
      if null passed
        then Left (r, failed)
        else Right (Just r)

checkRoutePrism :: (Eq r, Show r) => (a -> Prism_ FilePath r) -> a -> r -> FilePath -> Either Text ()
checkRoutePrism p a r s =
  let (valid, checkLog) =
        runWriter $ routePrismIsLawfulFor p a r s
   in if valid
        then Right ()
        else Left $ "Unlawful route prism for route value '" <> show r <> "'\n- " <> T.intercalate "\n- " checkLog

{- | Check if the route @Prism_@ is lawful.

  A route @Prism_@ is lawful if its conversions both the ways form an
  isomorphism for a given value.

  Returns a Writer reporting logs.
-}
routePrismIsLawfulFor ::
  forall ctx a.
  (Eq a, Show a) =>
  (ctx -> Prism_ FilePath a) ->
  ctx ->
  a ->
  FilePath ->
  Writer [Text] Bool
routePrismIsLawfulFor enc =
  prismIsLawfulFor . fromPrism_ . enc

prismIsLawfulFor ::
  forall s a.
  (Eq a, Eq s, Show a, ToText s) =>
  Prism' s a ->
  a ->
  s ->
  Writer [Text] Bool
prismIsLawfulFor p a s = do
  -- TODO: The logging here could be improved.
  -- log $ "Testing Partial ISO law for " <> show a <> " and " <> toText s
  let s' :: s = review p a
  -- log $ "Prism actual encoding: " <> toText s'
  let ma' :: Maybe a = preview p s'
  -- log $ "Decoding of that encoding: " <> show ma'
  unless (s == s') $
    log $
      toText s <> " /= " <> toText s' <> " (encoding of '" <> show a <> "')"
  unless (Just a == ma') $
    log $
      show (Just a) <> " /= " <> show ma' <> " (decoding of " <> toText s <> ")"
  pure $ (s == s') && (Just a == ma')
  where
    log = tell . one
