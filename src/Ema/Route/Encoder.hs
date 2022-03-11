{-# LANGUAGE InstanceSigs #-}

module Ema.Route.Encoder
  ( -- * Route encoder
    RouteEncoder,
    unsafeMkRouteEncoder,
    encodeRoute,
    decodeRoute,
    allRoutes,
    singletonRouteEncoder,
    singletonRouteEncoderFrom,
    isoRouteEncoder,
    showReadRouteEncoder,
    mapRouteEncoder,
    leftRouteEncoder,
    rightRouteEncoder,
    mergeRouteEncoder,

    -- * Internal
    checkRouteEncoderForSingleRoute,
    -- PartialIsoFunctor (pimap),
  )
where

import Control.Lens (Iso, Prism')
import Control.Lens qualified as Lens
import Control.Monad.Writer
import Data.Text qualified as T

-- | An Iso that is not necessarily surjective; as well as takes an (unchanging)
-- context value.
--
-- Parse `s` into (optional) `a` which can always be converted to a `s`. The `a`
-- can be enumerated finitely. `ctx` is used to all functions.
-- TODO: Is this isomrophic to `Iso (ctx, a) (Maybe a) s (ctx, s)` (plus, `ctx -> [a]`)?
-- TODO: replace `ctx` arg with Reader monad?
newtype PartialIsoEnumerableWithCtx ctx s a
  = PartialIsoEnumerableWithCtx (ctx -> a -> s, ctx -> s -> Maybe a, ctx -> [a])

{-
type T ctx a s = CtxIso ctx a (Maybe a) s s

type CtxIso ctx a b c d = Iso (ctx, a) b c (ctx, d)

type T' ctx a = ctx -> [a]
-}

partialIsoIsLawfulFor :: (Eq a, Eq s, Show a, ToText s) => PartialIsoEnumerableWithCtx ctx s a -> ctx -> a -> s -> Writer [Text] Bool
partialIsoIsLawfulFor (PartialIsoEnumerableWithCtx (to, from, _)) ctx a s = do
  tell . one $ "Testing Partial ISO law for " <> show a <> " and " <> toText s
  let s' = to ctx a
  tell . one $ "Route's actual encoding: " <> toText s'
  let ma' = from ctx s'
  tell . one $ "Decoding of that encoding: " <> show ma'
  unless (s == s') $
    tell . one $ "ERR: " <> toText s <> " /= " <> toText s'
  unless (Just a == ma') $
    tell . one $ "ERR: " <> show (Just a) <> " /= " <> show ma'
  pure $ (s == s') && (Just a == ma')

class PartialIsoFunctor (f :: Type -> Type -> Type -> Type) where
  pimap ::
    Iso a (Maybe a) b b ->
    Prism' c d ->
    (y -> x) ->
    f x a c ->
    f y b d

instance PartialIsoFunctor PartialIsoEnumerableWithCtx where
  pimap ::
    forall a b c d x y.
    Iso a (Maybe a) b b ->
    Prism' c d ->
    (y -> x) ->
    PartialIsoEnumerableWithCtx x a c ->
    PartialIsoEnumerableWithCtx y b d
  pimap iso1 iso2 h (PartialIsoEnumerableWithCtx (enc, dec, all_)) =
    PartialIsoEnumerableWithCtx (enc', dec', all_')
    where
      enc' :: y -> d -> b
      enc' m r =
        let r' :: c = Lens.review iso2 r
            m' :: x = h m
         in Lens.withIso iso1 $ \f _ -> f $ enc m' r'
      dec' :: y -> b -> Maybe d
      dec' m fp = do
        fp' <- Lens.withIso iso1 $ \_ f -> f fp
        r :: c <- dec (h m) fp'
        Lens.preview iso2 r
      all_' :: y -> [d]
      all_' m =
        mapMaybe (Lens.preview iso2) (all_ $ h m)

newtype RouteEncoder a r = RouteEncoder (PartialIsoEnumerableWithCtx a FilePath r)

mapRouteEncoder ::
  Iso FilePath (Maybe FilePath) FilePath FilePath ->
  Prism' r1 r2 ->
  (b -> a) ->
  RouteEncoder a r1 ->
  RouteEncoder b r2
mapRouteEncoder fpIso rIso mf (RouteEncoder enc) =
  RouteEncoder $ pimap fpIso rIso mf enc

unsafeMkRouteEncoder :: (ctx -> a -> FilePath) -> (ctx -> FilePath -> Maybe a) -> (ctx -> [a]) -> RouteEncoder ctx a
unsafeMkRouteEncoder x y z = RouteEncoder $ PartialIsoEnumerableWithCtx (x, y, z)

encodeRoute :: RouteEncoder model r -> model -> r -> FilePath
encodeRoute (RouteEncoder (PartialIsoEnumerableWithCtx (f, _, _))) = f

decodeRoute :: RouteEncoder model r -> model -> FilePath -> Maybe r
decodeRoute (RouteEncoder (PartialIsoEnumerableWithCtx (_, f, _))) = f

allRoutes :: RouteEncoder model r -> model -> [r]
allRoutes (RouteEncoder (PartialIsoEnumerableWithCtx (_, _, f))) = f

-- | Returns a new route encoder that supports either of the input routes.
mergeRouteEncoder :: RouteEncoder a r1 -> RouteEncoder b r2 -> RouteEncoder (a, b) (Either r1 r2)
mergeRouteEncoder enc1 enc2 =
  unsafeMkRouteEncoder
    ( \m ->
        either
          (encodeRoute enc1 (fst m))
          (encodeRoute enc2 (snd m))
    )
    ( \m fp ->
        asum
          [ Left <$> decodeRoute enc1 (fst m) fp,
            Right <$> decodeRoute enc2 (snd m) fp
          ]
    )
    ( \m ->
        mconcat
          [ Left <$> allRoutes enc1 (fst m),
            Right <$> allRoutes enc2 (snd m)
          ]
    )

-- | TODO: Can do this using generics, on any `f` (not just Either)
--
-- But we as well need model lens for each inner route. Unless we use heterogenous list?
leftRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder a r1
leftRouteEncoder =
  mapRouteEncoder
    (Lens.iso id Just)
    (Lens.prism' Left leftToMaybe)
    (,undefined)

rightRouteEncoder :: RouteEncoder (a, b) (Either r1 r2) -> RouteEncoder b r2
rightRouteEncoder =
  mapRouteEncoder
    (Lens.iso id Just)
    (Lens.prism' Right rightToMaybe)
    (undefined,)

singletonRouteEncoderFrom :: FilePath -> RouteEncoder a ()
singletonRouteEncoderFrom fp =
  unsafeMkRouteEncoder (const enc) (const dec) (const all_)
  where
    enc () = fp
    dec fp' = guard (fp' == fp)
    all_ = [()]

isoRouteEncoder :: Iso r (Maybe r) FilePath FilePath -> RouteEncoder () r
isoRouteEncoder iso =
  unsafeMkRouteEncoder
    (const $ \r -> Lens.withIso iso $ \f _ -> f r)
    (const $ \fp -> Lens.withIso iso $ \_ g -> g fp)
    (const [])

showReadRouteEncoder :: (Show r, Read r) => RouteEncoder () r
showReadRouteEncoder =
  unsafeMkRouteEncoder (const enc) (const dec) (const [])
  where
    enc r = show r <> ".html"
    dec fp = do
      x' <- fmap toString $ T.stripSuffix ".html" $ toText fp
      readMaybe x'

-- | Route encoder for single route encoding to 'index.html'
singletonRouteEncoder :: RouteEncoder a ()
singletonRouteEncoder =
  singletonRouteEncoderFrom "index.html"

checkRouteEncoderForSingleRoute :: (Eq route, Show route) => RouteEncoder model route -> model -> route -> FilePath -> Writer [Text] Bool
checkRouteEncoderForSingleRoute (RouteEncoder piso) = partialIsoIsLawfulFor piso
