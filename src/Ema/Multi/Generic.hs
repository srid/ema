{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic where

import Data.SOP (I (..), NP (..), NS (..))
import Ema.Multi
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder
import Ema.Route.Extra
import Optics.Core (iso)
import Optics.Prism (prism')

{- |
type family RCode (xss :: [[Type]]) :: [Type] where
  RCode '[] = '[]
  RCode ('[] ': rest) = Void ': RCode rest
  RCode ('[x] ': rest) = x ': RCode rest
  RCode (_ ': rest) = TypeError ( 'Text "MultiRoute: too many arguments")
-}

{- | Motley is a class of routes with an underlying MultiRoute (and MultiModel) representation.

 The idea is that by deriving Motley, we get IsRoute for free (based on MultiRoute).

 TODO: Rename this class, or change the API.
-}
class Motley r where
  -- | The model associated with `r`
  type MotleyModel r :: Type

  type MotleySubRoutes r :: [Type]
  toMultiR :: r -> MultiRoute (MotleySubRoutes r)
  fromMultiR :: MultiRoute (MotleySubRoutes r) -> r
  toMultiM :: MotleyModel r -> NP I (MultiModel (MotleySubRoutes r))
  fromMultiM :: NP I (MultiModel (MotleySubRoutes r)) -> MotleyModel r

-- | Mark a route as associated with a model type.
newtype WithModel r a = WithModel r

instance Motley r => Motley (WithModel r a) where
  type MotleyModel (WithModel r a) = MotleyModel r
  type MotleySubRoutes (WithModel r a) = MotleySubRoutes r
  toMultiR (WithModel r) = toMultiR @r r
  fromMultiR = WithModel . fromMultiR @r
  toMultiM = toMultiM @r
  fromMultiM = fromMultiM @r

instance
  ( Motley r
  , mr ~ MultiRoute (MotleySubRoutes r)
  , mm ~ MultiModel (MotleySubRoutes r)
  , a ~ MotleyModel r
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  ) =>
  IsRoute (WithModel r a)
  where
  type RouteModel (WithModel r a) = a
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoderRoute (iso fromMultiR toMultiR)
      & mapRouteEncoderModel (toMultiM @r)
  allRoutes m =
    WithModel . fromMultiR
      <$> allRoutes (toMultiM @r m)

-- ----------
-- Examples
-- ----------

type M = (Int, Int, String)

data R = R_Main | R_Foo | R_Bar NumRoute | R_Bar2 NumRoute
  deriving (IsRoute) via (WithModel R M) -- This only works if MotleyModel R ~ M

data NumRoute = NumRoute

instance IsRoute NumRoute where
  type RouteModel NumRoute = Int
  routeEncoder = mkRouteEncoder $ \n ->
    prism' (const $ show n <> ".html") $ \s -> do
      guard $ s == show n <> ".html"
      pure NumRoute
  allRoutes _ = [NumRoute]

-- TODO: We want to derive Motley generically.
instance Motley R where
  type MotleyModel R = M
  type
    MotleySubRoutes R =
      '[ SingletonRoute "main.html"
       , SingletonRoute "foo.html"
       , PrefixedRoute "bar" NumRoute
       , PrefixedRoute "bar2" NumRoute
       ]
  toMultiR = \case
    R_Main -> Z $ I SingletonRoute
    R_Foo -> S $ Z $ I SingletonRoute
    R_Bar r -> S $ S $ Z $ I $ PrefixedRoute r
    R_Bar2 r -> S $ S $ S $ Z $ I $ PrefixedRoute r
  fromMultiR = \case
    Z (I SingletonRoute) -> R_Main
    S (Z (I SingletonRoute)) -> R_Foo
    S (S (Z (I (PrefixedRoute r)))) -> R_Bar r
    S (S (S (Z (I (PrefixedRoute r))))) -> R_Bar2 r
    S (S (S (S _))) -> error "FIXME" -- not reachable
  toMultiM (a, b, _) =
    I () :* I () :* I a :* I b :* Nil

  -- XXX: We may not need this after all (not used so far). But if we do, then
  -- note the undefined 'fillers'.
  fromMultiM (I () :* I () :* I a :* I b :* Nil) =
    (a, b, undefined)
