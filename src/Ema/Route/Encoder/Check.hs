module Ema.Route.Encoder.Check (
  checkRouteEncoderGivenFilePath,
  checkRouteEncoderGivenRoute,
) where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Text qualified as T
import Ema.Route.Encoder.Type (RouteEncoder, applyRouteEncoder)
import Optics.Core (Prism', preview, review)
import System.FilePath ((</>))

checkRouteEncoderGivenRoute :: (HasCallStack, Eq r, Show r) => RouteEncoder a r -> a -> r -> Either Text ()
checkRouteEncoderGivenRoute enc a r =
  let s = review (applyRouteEncoder enc a) r
   in checkRouteEncoder enc a r s

checkRouteEncoderGivenFilePath ::
  (HasCallStack, Eq r, Show r) =>
  RouteEncoder a r ->
  a ->
  FilePath ->
  Either (r, [(FilePath, Text)]) (Maybe r)
checkRouteEncoderGivenFilePath enc a s = do
  -- We should treat /foo, /foo.html and /foo/index.html as equivalent.
  let candidates = [s, s <> ".html", s </> "index.html"]
      rp = applyRouteEncoder enc a
  case asum (preview rp <$> candidates) of
    Nothing -> pure Nothing
    Just r -> do
      -- All candidates must be checked, and if even one passes - we let this
      -- route go through.
      let (failed, passed) =
            partitionEithers $
              candidates <&> \candidate ->
                case checkRouteEncoder enc a r candidate of
                  Left err -> Left (candidate, err)
                  Right () -> Right ()
      if null passed
        then Left (r, failed)
        else Right (Just r)

checkRouteEncoder :: (Eq r, Show r) => RouteEncoder a r -> a -> r -> FilePath -> Either Text ()
checkRouteEncoder p a r s =
  let (valid, checkLog) =
        runWriter $ routeEncoderIsLawfulFor p a r s
   in if valid
        then Right ()
        else Left $ "Encoding for route '" <> show r <> "' is not isomorphic:\n - " <> T.intercalate "\n - " checkLog

{- | Check if the @CtxPrism@ is lawful.

  A @CtxPrism@ is lawful if its conversions both the ways form an isomorphism
  for a given value.

  Returns a Writer reporting logs.
-}
routeEncoderIsLawfulFor ::
  forall ctx a.
  (Eq a, Show a) =>
  RouteEncoder ctx a ->
  ctx ->
  a ->
  FilePath ->
  Writer [Text] Bool
routeEncoderIsLawfulFor enc =
  prismIsLawfulFor . applyRouteEncoder enc

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
