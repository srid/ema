{-# LANGUAGE DeriveAnyClass #-}

module Ema.Route.Lib.Extra.MapRoute (
  MapRoute (MapRoute),
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (fromPrism_, toPrism_)
import Generics.SOP qualified as SOP
import Optics.Core (preview, prism', review)

{- | Like `FolderRoute` but using dynamic folder name, looked up on a `Map`.

  Empty folder name (map keys) are supported. They would translate in effect to
  the looked up route (no folder created).
-}
newtype MapRoute k r = MapRoute (k, r)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (IsRoute r, IsString k, ToString k, Ord k, Show r) => IsRoute (MapRoute k r) where
  type RouteModel (MapRoute k r) = Map k (RouteModel r)
  routePrism rs =
    -- TODO: Reimplement using optics composition
    toPrism_ $
      prism'
        ( \(MapRoute (k, r)) ->
            let m = fromJust $ Map.lookup k rs -- HACK: fromJust
                prefix = if toString k == "" then "" else toString k <> "/"
             in prefix <> review (fromPrism_ $ routePrism @r m) r
        )
        ( \fp -> do
            let candidates =
                  case breakPath fp of
                    (a, Nothing) -> [("", a)]
                    (a, Just b) -> [(a, b), ("", fp)]
            (m, k, rest) <-
              asum $
                candidates <&> \(base, rest) ->
                  let k = fromString base
                   in (,k,rest) <$> Map.lookup k rs
            r <- preview (fromPrism_ $ routePrism @r m) (toString rest)
            pure $ MapRoute (k, r)
        )
    where
      -- Breaks a path once on the first slash.
      breakPath :: (HasCallStack) => String -> (String, Maybe [Char])
      breakPath (s :: String) =
        case T.breakOn "/" (toText s) of
          (p, "") -> (toString p, Nothing)
          (p, toString -> '/' : rest) -> (toString p, Just rest)
          _ -> error "T.breakOn: impossible"

  routeUniverse rs = concatMap (\(a, m) -> MapRoute . (a,) <$> routeUniverse m) $ Map.toList rs
