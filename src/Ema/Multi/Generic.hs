{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic where

import Data.SOP (I (..), NP (..), NS (..))
import Ema.App qualified as Ema
import Ema.Asset qualified as Asset
import Ema.Multi
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder
import Ema.Route.Extra
import Ema.Site
import Optics.Core (iso)
import Optics.Prism (prism')

{- |
type family RCode (xss :: [[Type]]) :: [Type] where
  RCode '[] = '[]
  RCode ('[] ': rest) = Void ': RCode rest
  RCode ('[x] ': rest) = x ': RCode rest
  RCode (_ ': rest) = TypeError ( 'Text "MultiRoute: too many arguments")
-}

{- | MotleyRoute is a class of routes with an underlying MultiRoute (and MultiModel) representation.

 The idea is that by deriving MotleyRoute (and MotleyModel), we get IsRoute for free (based on MultiRoute).

 TODO: Rename this class, or change the API.
-}
class MotleyRoute r where
  -- | The model associated with `r`
  type MotleyRouteSubRoutes r :: [Type]

  toMultiR :: r -> MultiRoute (MotleyRouteSubRoutes r)
  fromMultiR :: MultiRoute (MotleyRouteSubRoutes r) -> r

class MotleyRoute r => MotleyModel r where
  type MotleyModelType r :: Type
  toMultiM :: MotleyModelType r -> NP I (MultiModel (MotleyRouteSubRoutes r))
  _fromMultiM :: NP I (MultiModel (MotleyRouteSubRoutes r)) -> MotleyModelType r

-- | Mark a route as associated with a model type.
newtype WithModel r a = WithModel r

instance MotleyRoute r => MotleyRoute (WithModel r a) where
  type MotleyRouteSubRoutes (WithModel r a) = MotleyRouteSubRoutes r
  toMultiR (WithModel r) = toMultiR @r r
  fromMultiR = WithModel . fromMultiR @r

instance MotleyModel r => MotleyModel (WithModel r a) where
  type MotleyModelType (WithModel r a) = MotleyModelType r
  toMultiM = toMultiM @r
  _fromMultiM = _fromMultiM @r

instance
  ( MotleyRoute r
  , MotleyModel r
  , mr ~ MultiRoute (MotleyRouteSubRoutes r)
  , mm ~ MultiModel (MotleyRouteSubRoutes r)
  , a ~ MotleyModelType r
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
  deriving stock (Show, Eq)
  deriving (IsRoute) via (WithModel R M) -- This only works if MotleyModelType R ~ M

data NumRoute = NumRoute
  deriving stock (Show, Eq)

instance IsRoute NumRoute where
  type RouteModel NumRoute = Int
  routeEncoder = mkRouteEncoder $ \n ->
    let fp = show n <> ".html"
     in prism' (const fp) $ \s -> do
          guard $ s == fp
          pure NumRoute
  allRoutes _ = [NumRoute]

-- TODO: We want to derive MotleyRoute generically.
instance MotleyRoute R where
  type
    MotleyRouteSubRoutes R =
      '[ SingletonRoute "index.html"
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

-- TODO: In many simple cases (such as single model cases) this can be derived
-- generically. But allow the user to define this manually if need be. Also cf.
-- Sub-type. https://hackage.haskell.org/package/records-sop-0.1.1.0/docs/Generics-SOP-Record-SubTyping.html
-- Unlike sub-type, we must support a `NP` as the 'super'-type (not records).
instance MotleyModel R where
  type MotleyModelType R = M
  toMultiM (a, b, _) =
    -- In the single-model case this would be roughly same as: npConstFrom
    I () :* I () :* I a :* I b :* Nil

  -- XXX: We may not need this after all (not used so far). But if we do, then
  -- note the undefined 'fillers'.
  _fromMultiM (I () :* I () :* I a :* I b :* Nil) =
    (a, b, undefined)

instance EmaSite R where
  siteInput _ _ () = pure $ pure (42, 21, "random")
  siteOutput _ m r = Asset.AssetGenerated Asset.Html $ show r <> show m

main :: IO ()
main =
  Ema.runSite_ @R ()
