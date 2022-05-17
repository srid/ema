-- | TODO: Export only what's necessary.
module Ema.Route.Encoder where

import Control.Monad.Writer (runWriter)
import Data.Text qualified as T
import Optics.Core (
  A_Prism,
  Is,
  NoIx,
  Optic',
  Prism',
  castOptic,
  equality,
  iso,
  prism',
 )
import Optics.CtxPrism (
  CtxPrism,
  cpmap,
  cpreview,
  creview,
  ctxPrismIsLawfulFor,
  fromPrism,
 )
import System.FilePath ((</>))

{- | An encoder cum decoder that knows how to convert routes to and from
 filepaths. The conversion depends on the context `a`.
-}
newtype RouteEncoder a r = RouteEncoder (CtxPrism a FilePath r)

-- | Make a `RouteEncoder` manually.
mkRouteEncoder :: (a -> Prism' FilePath r) -> RouteEncoder a r
mkRouteEncoder = RouteEncoder . fromPrism

encodeRoute :: RouteEncoder model r -> model -> r -> FilePath
encodeRoute (RouteEncoder enc) = creview enc

decodeRoute :: RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder enc) = cpreview enc

mapRouteEncoder ::
  (pr `Is` A_Prism, pf `Is` A_Prism) =>
  Optic' pf NoIx FilePath FilePath ->
  Optic' pr NoIx r1 r2 ->
  (b -> a) ->
  RouteEncoder a r1 ->
  RouteEncoder b r2
mapRouteEncoder fp r m (RouteEncoder enc) =
  RouteEncoder $ cpmap (castOptic fp) (castOptic r) m enc

-- | Like `mapRouteEncoder` but maps only the route
mapRouteEncoderRoute :: pr `Is` A_Prism => Optic' pr NoIx r1 r2 -> RouteEncoder a r1 -> RouteEncoder a r2
mapRouteEncoderRoute f = mapRouteEncoder equality f id

-- | Like `mapRouteEncoder` but maps only the encoded FilePath
mapRouteEncoderFilePath :: pr `Is` A_Prism => Optic' pr NoIx FilePath FilePath -> RouteEncoder a r -> RouteEncoder a r
mapRouteEncoderFilePath f = mapRouteEncoder f equality id

-- | Like `mapRouteEncoder` but (contra)maps the model
mapRouteEncoderModel :: (b -> a) -> RouteEncoder a r2 -> RouteEncoder b r2
mapRouteEncoderModel = mapRouteEncoder equality equality

-- | Like `mkRouteEncoder` but ignores the context
prismRouteEncoder :: forall r a. Prism' FilePath r -> RouteEncoder a r
prismRouteEncoder = mkRouteEncoder . const

-- | A route encoder that uses Show/Read to encode/decode.
showReadRouteEncoder :: (Show r, Read r) => RouteEncoder a r
showReadRouteEncoder =
  htmlSuffixEncoder
    & mapRouteEncoderRoute (prism' show readMaybe)

-- | A route encoder that uses toString/fromString to encode/decode.
stringRouteEncoder :: (IsString r, ToString r) => RouteEncoder a r
stringRouteEncoder =
  htmlSuffixEncoder
    & mapRouteEncoderRoute (iso fromString toString)

-- | An encoder that uses the ".html" suffix
htmlSuffixEncoder :: RouteEncoder a FilePath
htmlSuffixEncoder =
  prismRouteEncoder $ prism' (<> ".html") (fmap toString . T.stripSuffix ".html" . toText)

singletonRouteEncoderFrom :: FilePath -> RouteEncoder a ()
singletonRouteEncoderFrom fp =
  prismRouteEncoder $ prism' (\() -> fp) (\s -> guard (s == fp))

-- | Route encoder for single route encoding to 'index.html'
singletonRouteEncoder :: RouteEncoder a ()
singletonRouteEncoder =
  singletonRouteEncoderFrom "index.html"

checkRouteEncoderGivenRoute :: (HasCallStack, Eq r, Show r) => RouteEncoder a r -> a -> r -> Either Text ()
checkRouteEncoderGivenRoute enc a r =
  let s = encodeRoute enc a r
   in checkRouteEncoder enc a r s

checkRouteEncoderGivenFilePath ::
  (HasCallStack, Eq r, Show r) =>
  RouteEncoder a r ->
  a ->
  FilePath ->
  Either ((FilePath, r), Text) (Maybe r)
checkRouteEncoderGivenFilePath enc a s = do
  -- We should treat /foo, /foo.html and /foo/index.html as equivalent.
  --
  -- The order here matters: The .html ending ones must be checked, inasmuch as
  -- that is what a legitimate route encoder would encode to (i.e, a HTML
  -- filepath).
  let candidates = [s </> "index.html", s <> ".html", s]
  case asum (decodeRouteWithInput enc a <$> candidates) of
    Nothing -> pure Nothing
    Just (candidate, r) -> do
      case checkRouteEncoder enc a r candidate of
        Left err -> Left ((candidate, r), err)
        Right () -> pure (Just r)
  where
    decodeRouteWithInput enc' a' s' = do
      r <- decodeRoute enc' a' s'
      pure (s', r)

checkRouteEncoder :: (Eq r, Show r) => RouteEncoder a r -> a -> r -> FilePath -> Either Text ()
checkRouteEncoder (RouteEncoder p) a r s =
  let (valid, checkLog) =
        runWriter $ ctxPrismIsLawfulFor p a r s
   in if valid
        then Right ()
        else Left $ "Encoding for route '" <> show r <> "' is not isomorphic:\n - " <> T.intercalate "\n - " checkLog

-- | Returns a new route encoder that supports either of the input routes.
mergeRouteEncoder :: RouteEncoder a r1 -> RouteEncoder b r2 -> RouteEncoder (a, b) (Either r1 r2)
mergeRouteEncoder enc1 enc2 =
  -- TODO: this can be made safe, using lens composition.
  mkRouteEncoder $ \m ->
    prism'
      ( either
          (encodeRoute enc1 (fst m))
          (encodeRoute enc2 (snd m))
      )
      ( \fp ->
          asum
            [ Left <$> decodeRoute enc1 (fst m) fp
            , Right <$> decodeRoute enc2 (snd m) fp
            ]
      )

leftRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder a r1
leftRouteEncoder =
  mapRouteEncoder
    equality
    (prism' Left leftToMaybe)
    (,willNotBeUsed)

rightRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder b r2
rightRouteEncoder =
  mapRouteEncoder
    equality
    (prism' Right rightToMaybe)
    (willNotBeUsed,)

-- FIXME: a design hack necessitates it.
willNotBeUsed :: HasCallStack => a
willNotBeUsed = error "This value will not be used"
