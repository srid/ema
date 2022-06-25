{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Multi.Generic.Example where

import Data.Generics.Sum.Any (AsAny (_As))
import Data.SOP (I (..), NP (..), NS (..))
import Ema.App qualified as Ema
import Ema.Asset qualified as Asset
import Ema.Multi.Generic
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Encoder
import Ema.Route.Extra
import Ema.Site
import Optics.Core (iso)
import Optics.Prism (prism')

-- ----------
-- Examples
-- ----------

type M = (Int, Int, String)

data R = R_Index | R_Foo | R_Bar NumRoute | R_Bar2 NumRoute
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
  motleyRouteIso =
    iso
      ( \case
          R_Index -> Z $ coerce ()
          R_Foo -> S $ Z $ coerce ()
          R_Bar r -> S $ S $ Z $ coerce r
          R_Bar2 r -> S $ S $ S $ Z $ coerce r
      )
      ( \case
          Z (coerce -> ()) -> R_Index
          S (Z (coerce -> ())) -> R_Foo
          S (S (Z r)) -> R_Bar $ coerce r
          S (S (S (Z r))) -> R_Bar2 $ coerce r
          S (S (S (S x))) -> case x of {}
      )

-- TODO: In many simple cases (such as single model cases) this can be derived
-- generically. But allow the user to define this manually if need be. Also cf.
-- Sub-type. https://hackage.haskell.org/package/records-sop-0.1.1.0/docs/Generics-SOP-Record-SubTyping.html
-- Unlike sub-type, we must support a `NP` as the 'super'-type (not records).
instance MotleyModel R where
  type MotleyModelType R = M
  motleySubModels (a, b, _) =
    -- In the single-model case this would be roughly same as: npConstFrom
    I () :* I () :* I a :* I b :* Nil

instance EmaSite R where
  siteInput _ _ () = pure $ pure (42, 21, "inner")
  siteOutput _ m r = Asset.AssetGenerated Asset.Html $ show r <> show m

-- --warnings -c "cabal repl ema -f with-examples" -T Ema.Multi.Generic.main  --setup ":set args gen /tmp"
main :: IO ()
main = Ema.runSite_ @R ()

-- ---
-- Let's try defining a top-level route using `R` to see how EmaSite instances compose.
-- --

type TM = (M, String)

data TR = TR_Index | TR_Inner R
  deriving stock (Show, Eq, Generic)
  deriving (IsRoute) via (WithModel TR TM) -- This only works if MotleyModelType R ~ M

instance MotleyRoute TR where
  type MotleyRouteSubRoutes TR = '[SingletonRoute "index.html", PrefixedRoute "inner" R]
  motleyRouteIso =
    iso
      ( \case
          TR_Index -> Z $ coerce ()
          TR_Inner r -> S $ Z $ coerce r
      )
      ( \case
          Z (I (coerce -> ())) -> TR_Index
          S (Z r) -> TR_Inner $ coerce r
          S (S x) -> case x of {}
      )

instance MotleyModel TR where
  type MotleyModelType TR = TM
  motleySubModels (m, _) =
    I () :* I m :* Nil

instance EmaSite TR where
  siteInput x enc () = do
    m1 <- siteInput @R x (trInnerEnc enc) ()
    pure $ fmap (,"TOP") m1
  siteOutput enc m = \case
    r@TR_Index ->
      Asset.AssetGenerated Asset.Html $ show r <> show m
    TR_Inner r ->
      -- Might as well provide a `innerSiteOutput (_As @TR_Inner)`?
      siteOutput @R (trInnerEnc enc) (trInnerModel m) r

-- TODO: General version of this (cf. innerRouteEncoder)
trInnerEnc enc =
  enc
    & mapRouteEncoderRoute (_As @"TR_Inner")
    & mapRouteEncoderModel (,undefined)

-- TODO: General version of this (cf. innerModel)
trInnerModel m =
  let I () :* I m' :* Nil = motleySubModels @TR m
   in m'

mainTop :: IO ()
mainTop = Ema.runSite_ @TR ()

-- ---
-- Ensure that the constant model case (simple one) still works
-- --

type M2 = (Int, String)

data R2 = R2_Index | R2_Foo | R2_Bar BarRoute | R2_Bar2 BarRoute
  deriving stock (Show, Eq)
  deriving (IsRoute, MotleyModel) via (WithConstModel R2 M2)

data BarRoute = BarRoute
  deriving stock (Show, Eq)
  deriving (IsRoute, MotleyModel) via (WithConstModel BarRoute M2)

instance MotleyRoute BarRoute where
  type MotleyRouteSubRoutes BarRoute = '[SingletonRoute "index.html"]
  motleyRouteIso =
    iso
      ( \case
          BarRoute -> Z $ coerce ()
      )
      ( \case
          Z (coerce -> ()) -> BarRoute
          S x -> case x of {}
      )

instance MotleyRoute R2 where
  type MotleyRouteSubRoutes R2 = '[SingletonRoute "index.html", SingletonRoute "foo", PrefixedRoute "bar" BarRoute, PrefixedRoute "bar2" BarRoute]
  motleyRouteIso =
    iso
      ( \case
          R2_Index -> Z $ coerce ()
          R2_Foo -> S $ Z $ coerce ()
          R2_Bar r -> S $ S $ Z $ coerce r
          R2_Bar2 r -> S $ S $ S $ Z $ coerce r
      )
      ( \case
          Z (coerce -> ()) -> R2_Index
          S (Z (coerce -> ())) -> R2_Foo
          S (S (Z r)) -> R2_Bar $ coerce r
          S (S (S (Z r))) -> R2_Bar2 $ coerce r
          S (S (S (S x))) -> case x of {}
      )

instance EmaSite R2 where
  siteInput _ _ () = pure $ pure (21, "inner")
  siteOutput _ m r = Asset.AssetGenerated Asset.Html $ show r <> show m

mainConst :: IO ()
mainConst = Ema.runSite_ @R2 ()
