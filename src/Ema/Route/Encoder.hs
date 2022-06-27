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

encodeRoute :: HasCallStack => RouteEncoder model r -> model -> r -> FilePath
encodeRoute (RouteEncoder enc) = creview enc

decodeRoute :: HasCallStack => RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder enc) = cpreview enc

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

-- | A route encoder that uses @toString@ and @fromString@ to encode and decode respectively.
stringRouteEncoder :: (IsString r, ToString r) => RouteEncoder a r
stringRouteEncoder =
  htmlSuffixEncoder
    & mapRouteEncoderRoute (iso fromString toString)

-- | An encoder that uses the ".html" suffix
htmlSuffixEncoder :: RouteEncoder a FilePath
htmlSuffixEncoder =
  prismRouteEncoder htmlSuffixPrism

htmlSuffixPrism :: Prism' FilePath FilePath
htmlSuffixPrism = prism' (<> ".html") (fmap toString . T.stripSuffix ".html" . toText)

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
  Either (r, [(FilePath, Text)]) (Maybe r)
checkRouteEncoderGivenFilePath enc a s = do
  -- We should treat /foo, /foo.html and /foo/index.html as equivalent.
  let candidates = [s, s <> ".html", s </> "index.html"]
  case asum (decodeRoute enc a <$> candidates) of
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
checkRouteEncoder (RouteEncoder p) a r s =
  let (valid, checkLog) =
        runWriter $ ctxPrismIsLawfulFor p a r s
   in if valid
        then Right ()
        else Left $ "Encoding for route '" <> show r <> "' is not isomorphic:\n - " <> T.intercalate "\n - " checkLog

{- | Returns a new @RouteEncoder@ that supports *either* of the input routes.

  The resulting @RouteEncoder@'s model type becomes the *product* of the input models.
-}
eitherRouteEncoder :: RouteEncoder a r1 -> RouteEncoder b r2 -> RouteEncoder (a, b) (Either r1 r2)
eitherRouteEncoder enc1 enc2 =
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

{- | Combine two `RouteEncoder`s into a single one

  Use the given mapping functions to transform to (or from) resultant model and
  route types.

  This is equivalent to `eitherRouteEncoder` except the resultant RouteEncoder
  can use arbitrary model and route types (instead of `(,)` and `Either`) .
-}
combineRouteEncoder ::
  forall m r pr m1 m2 r1 r2.
  Is pr A_Prism =>
  -- | Isomorphism between either of the input routes and the output route.
  Optic' pr NoIx (Either r1 r2) r ->
  -- | How to retrieve the input models given the output model.
  (m -> (m1, m2)) ->
  RouteEncoder m1 r1 ->
  RouteEncoder m2 r2 ->
  RouteEncoder m r
combineRouteEncoder rf mf enc1 enc2 =
  eitherRouteEncoder enc1 enc2
    & mapRouteEncoderRoute rf
    & mapRouteEncoderModel mf
