{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Ema.Route.Generic.Example where

import Data.Generics.Sum.Any (AsAny (_As))
import Data.SOP (I (..), NP (..))
import Ema
import Ema.Asset qualified as Asset
import Ema.Route.Generic
import Ema.Route.Generic.TH
import Ema.Route.Lib.File
import Generics.SOP qualified as SOP
import Generics.SOP.TH qualified as SOP
import Optics.Core ((%))
import Optics.Prism (prism')

-- ----------
-- Examples
-- ----------

type M = (Int, Int, String)

data R = R_Index | R_Foo | R_Bar NumRoute | R_Bar2 NumRoute
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasSubRoutes, IsRoute) via (GenericRoute R '[WithModel M])

-- We must derive this manually due to ambiguity in field types (Int)
instance HasSubModels R where
  subModels (a, b, _) =
    I () :* I () :* I a :* I b :* Nil

data NumRoute = NumRoute
  deriving stock (Show, Eq, Generic)

instance IsRoute NumRoute where
  type RouteModel NumRoute = Int
  routeEncoder = mkRouteEncoder $ \n ->
    let fp = show n <> ".html"
     in prism' (const fp) $ \s -> do
          guard $ s == fp
          pure NumRoute
  allRoutes _ = [NumRoute]

instance EmaSite R where
  siteInput _ () = pure $ pure (42, 21, "inner")
  siteOutput _ m r = Asset.AssetGenerated Asset.Html $ show r <> show m

-- --warnings -c "cabal repl ema -f with-examples" -T Ema.Route.Generic.main  --setup ":set args gen /tmp"
main :: IO ()
main = Ema.runSite_ @R ()

-- ---
-- Let's try defining a top-level route using `R` to see how EmaSite instances compose.
-- --

type TM = (M, String)

data TR = TR_Index | TR_Inner R
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasSubRoutes, HasSubModels, IsRoute) via (GenericRoute TR '[WithModel TM])

instance EmaSite TR where
  siteInput x () = do
    m1 <- siteInput @R x ()
    pure $ fmap (,"TOP") m1
  siteOutput rp m = \case
    r@TR_Index ->
      Asset.AssetGenerated Asset.Html $ show r <> show m
    TR_Inner r ->
      -- Might as well provide a `innerSiteOutput (_As @TR_Inner)`?
      siteOutput @R (rp % _As @"TR_Inner") (trInnerModel m) r
    where
      trInnerModel model =
        let I () :* I m' :* Nil = subModels @TR model
         in m'

mainTop :: IO ()
mainTop = Ema.runSite_ @TR ()

-- ---
-- Ensure that the constant model case (simple one) still works
-- --

type M2 = (Int, String)

data R2 = R2_Index | R2_Foo | R2_Bar BarRoute | R2_Bar2 BarRoute
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasSubRoutes, HasSubModels, IsRoute) via (GenericRoute R2 '[WithModel M2])

data BarRoute = BarRoute
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasSubRoutes, HasSubModels, IsRoute) via (GenericRoute BarRoute '[WithModel M2])

instance EmaSite R2 where
  siteInput _ () = pure $ pure (21, "inner")
  siteOutput _ m r = Asset.AssetGenerated Asset.Html $ show r <> show m

mainConst :: IO ()
mainConst = Ema.runSite_ @R2 ()

-- ---
-- Ensure deriveIsRoute works with (partial) subroutes
-- --

data M3
  = M3
  deriving stock (Eq, Show, Generic)

data R3
  = R3_Sub R3_SubR
  | R3_Index

data R3_SubR
  = R3_SubR
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            R3_SubR
            '[ WithSubRoutes
                '[ FileRoute "example.html"
                 ]
             ]
        )

SOP.deriveGeneric ''R3
deriveIsRoute ''R3 ''M3 $ Just [''R3_SubR, ''()]

-- ---
-- Ensure deriveIsRoute works in a no-subroute case
-- --

data M4
  = M4
  deriving stock (Eq, Show, Generic)

data R4
  = R4_Index
  | R3_About

SOP.deriveGeneric ''R4
deriveIsRoute ''R4 ''M4 Nothing
