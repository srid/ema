{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Multi.Generic.RGeneric where

import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Generics.SOP
import Prelude hiding (All, Generic)

-- | Like `Generic` but for Route types only.
class Generic r => RGeneric r where
  type RCode r :: [Type]
  rfrom :: r -> NS I (RCode r)
  rto :: NS I (RCode r) -> r

instance (Generic r, RGeneric' (Code r), All RouteNP (Code r)) => RGeneric r where
  type RCode r = RCode' (Code r)
  rfrom = rfrom' @(Code r) . unSOP . from
  rto = to . SOP . rto' @(Code r)

class RGeneric' (xss :: [[Type]]) where
  type RCode' xss :: [Type]
  rfrom' :: NS (NP I) xss -> NS I (RCode' xss)
  rto' :: NS I (RCode' xss) -> NS (NP I) xss

instance RGeneric' '[] where
  type RCode' '[] = '[]
  rfrom' = \case {}
  rto' = \case {}

instance (RGeneric' xss, RouteNP xs) => RGeneric' (xs ': xss) where
  type RCode' (xs ': xss) = RouteNPType xs ': RCode' xss
  rfrom' = \case
    Z routeNP -> Z $ I $ fromRouteNP routeNP
    S rest -> S $ rfrom' @xss rest
  rto' = \case
    Z (I t) -> Z $ toRouteNP t
    S rest -> S $ rto' @xss rest

{- | Class of `NP` that can be used in a route tyupe.

    Ensures that the constructor can have 0 or 1 products only.
-}
class RouteNP (xs :: [Type]) where
  type RouteNPType xs :: Type
  fromRouteNP :: NP I xs -> RouteNPType xs
  toRouteNP :: RouteNPType xs -> NP I xs

instance RouteNP '[] where
  type RouteNPType '[] = ()
  fromRouteNP Nil = ()
  toRouteNP () = Nil

instance RouteNP (x ': '[]) where
  type RouteNPType (x ': '[]) = x
  fromRouteNP (I x :* Nil) = x
  toRouteNP x = I x :* Nil

instance (TypeError ( 'Text "MultiRoute: too many arguments")) => RouteNP (x ': x' ': xs) where
  type RouteNPType (x ': x' ': xs) = TypeError ( 'Text "MultiRoute: too many arguments")
  fromRouteNP _ = undefined
  toRouteNP _ = undefined
