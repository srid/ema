{-# LANGUAGE UndecidableInstances #-}

-- | WIP https://github.com/srid/ema/issues/92
module Ema.Multi.Generic where

import Data.SOP (I (..), NP (..), NS (..))
import Ema.Multi
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder
import Ema.Route.Extra
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Optics.Core (iso)
import Optics.Prism (prism')

{- |
type family RCode (xss :: [[Type]]) :: [Type] where
  RCode '[] = '[]
  RCode ('[] ': rest) = Void ': RCode rest
  RCode ('[x] ': rest) = x ': RCode rest
  RCode (_ ': rest) = TypeError ( 'Text "MultiRoute: too many arguments")
-}

-- | Class of routes (with their associated model) that are MultiRoute's
class AsMulti r a where
  type AsMultiSubRoutes r a :: [Type]
  toMultiR :: Proxy a -> r -> MultiRoute (AsMultiSubRoutes r a)
  fromMultiR :: Proxy a -> MultiRoute (AsMultiSubRoutes r a) -> r
  toMultiM :: Proxy r -> a -> NP I (MultiModel (AsMultiSubRoutes r a))
  fromMultiM :: Proxy r -> NP I (MultiModel (AsMultiSubRoutes r a)) -> a

newtype WithModel r a = WithModel r

instance AsMulti r a => AsMulti (WithModel r a) a where
  type AsMultiSubRoutes (WithModel r a) a = AsMultiSubRoutes r a
  toMultiR p (WithModel r) = toMultiR @r p r
  fromMultiR p = WithModel . fromMultiR @r p
  toMultiM _ = toMultiM @r Proxy
  fromMultiM _ = fromMultiM @r Proxy

instance
  ( AsMulti r a
  , mr ~ MultiRoute (AsMultiSubRoutes r a)
  , mm ~ MultiModel (AsMultiSubRoutes r a)
  , IsRoute mr
  , RouteModel mr ~ NP I mm
  ) =>
  IsRoute (WithModel r a)
  where
  type RouteModel (WithModel r a) = a
  routeEncoder =
    routeEncoder @mr
      & mapRouteEncoderRoute (iso (fromMultiR $ Proxy @a) (toMultiR $ Proxy @a))
      & mapRouteEncoderModel (toMultiM $ Proxy @r)
  allRoutes m =
    WithModel . fromMultiR (Proxy @a)
      <$> allRoutes (toMultiM (Proxy @r) m)

-- ----------
-- Examples
-- ----------

type M = (Int, Int)

data R = R_Main | R_Foo | R_Bar NumRoute | R_Bar2 NumRoute
  deriving (IsRoute) via (WithModel R M)

data NumRoute = NumRoute

instance IsRoute NumRoute where
  type RouteModel NumRoute = Int
  routeEncoder = mkRouteEncoder $ \n ->
    prism' (const $ show n <> ".html") $ \s -> do
      guard $ s == show n <> ".html"
      pure NumRoute
  allRoutes _ = [NumRoute]

-- TODO: We want to derive this generically.
instance AsMulti R M where
  type
    AsMultiSubRoutes R M =
      '[ SingletonRoute "main.html"
       , SingletonRoute "foo.html"
       , PrefixedRoute "bar" NumRoute
       , PrefixedRoute "bar2" NumRoute
       ]
  toMultiR _ = \case
    R_Main -> Z $ I SingletonRoute
    R_Foo -> S $ Z $ I SingletonRoute
    R_Bar r -> S $ S $ Z $ I $ PrefixedRoute r
    R_Bar2 r -> S $ S $ S $ Z $ I $ PrefixedRoute r
  fromMultiR _ = \case
    Z (I SingletonRoute) -> R_Main
    S (Z (I SingletonRoute)) -> R_Foo
    S (S (Z (I (PrefixedRoute r)))) -> R_Bar r
    S (S (S (Z (I (PrefixedRoute r))))) -> R_Bar2 r
    S (S (S (S _))) -> error "FIXME" -- not reachable
  toMultiM _ (a, b) =
    I () :* I () :* I a :* I b :* Nil
  fromMultiM _ (I () :* I () :* I a :* I b :* Nil) =
    (a, b)