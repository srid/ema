{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ema.Route.Generic.SubModel (
  HasSubModels (subModels),
  -- DerivingVia types
  GSubModels (..),
) where

import Data.Generics.Product (HasAny (the))
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Generic.SubRoute (HasSubRoutes (SubRoutes))
import Ema.Route.Lib.Multi (MultiModel)
import Generics.SOP (I (..), NP (Nil, (:*)))
import Optics.Core (united, view)
import Prelude hiding (All)

class (HasSubRoutes r) => HasSubModels r where
  -- | Break the model into a list of sub-models used correspondingly by the sub-routes.
  subModels :: RouteModel r -> NP I (MultiModel (SubRoutes r))

class GSubModels m (ms :: [Type]) (lookups :: [k]) where
  gsubModels :: m -> NP I ms

instance GSubModels m '[] '[] where
  gsubModels _ = Nil

instance
  {-# OVERLAPPING #-}
  (HasAny s m m t t, GSubModels m ms ss) =>
  GSubModels m (t ': ms) (s ': ss)
  where
  gsubModels m = I (view (the @s @m @_ @t @_) m) :* gsubModels @_ @m @ms @ss m

-- Useful instances to support varied types in `WithSubModels` list.

instance {-# OVERLAPPING #-} HasAny () s s () () where
  the = united

instance (HasAny sel s t a b) => HasAny (Proxy sel) s t a b where
  the = the @sel
