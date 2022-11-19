{-# LANGUAGE DeriveAnyClass #-}

module Ema.Route.Lib.Extra.StringRoute where

import Data.Map.Strict qualified as Map
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (htmlSuffixPrism, toPrism_)
import Generics.SOP qualified as SOP
import Optics.Core (coercedTo, iso, prism', (%))

{- | A route represented by a stringy type; associated with a Map indexed by
 same as its model.

 See Ex03_Store.hs for example usage.
-}
newtype StringRoute (a :: Type) r = StringRoute {unStringRoute :: r}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (IsString r, ToString r, Eq r, Ord r) => IsRoute (StringRoute a r) where
  type RouteModel (StringRoute a r) = Map r a
  routePrism as =
    toPrism_ $
      htmlSuffixPrism
        % iso fromString toString
        % mapMemberPrism as
        % coercedTo
    where
      mapMemberPrism m =
        prism' id $ \r -> r <$ guard (r `Map.member` m)
  routeUniverse as = StringRoute <$> Map.keys as
