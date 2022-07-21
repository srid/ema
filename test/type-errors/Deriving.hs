{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Deriving where

import Data.Proxy (Proxy)
import Data.Text (Text)
import Deriving.TH
import Ema.Route.Class
import Ema.Route.Generic
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Text.RawString.QQ (r)

-- * Helper data declarations

-- | Placeholder for anything deriving stock GHC.Generic
newtype Slug = Slug Text
  deriving stock (GHC.Generic)

-- | Nice model with named selectors deriving GHC.Generic
data NiceNamedM a b
  = M_Nice_Named 
      { niceNamed1 :: a
      , niceNamed2 :: b
      }
  deriving stock (GHC.Generic)

-- | Nice model with anonymous fields deriving GHC.Generic
data NiceAnonM a b
  = NiceAnonM a b
  deriving stock (GHC.Generic)

-- | Bad model; multi-constructor
data BadM a b = BadM1 a b | BadM2 a b
  deriving stock (GHC.Generic)

-- | Nice route with NiceNamedM () () as model, with empty constructor.
data PlainR_NiceNamedM
  = PlainR_NiceNamedM
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasSubRoutes, HasSubModels, IsRoute)
    via GenericRoute PlainR_NiceNamedM
      '[ WithModel (NiceNamedM () ())
       ]

-- | Nice route with NiceNamedM () () as model, wrapping something and deriving /newtype/
-- GHC.Generic.
newtype StockWrappedR_NiceNamedM a
  = StockWrappedR_NiceNamedM a
  deriving stock (Show, Eq, Ord, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance IsRoute (StockWrappedR_NiceNamedM a) where
  type RouteModel (StockWrappedR_NiceNamedM a) = NiceNamedM () ()
  routePrism = undefined
  routeUniverse = undefined

-- | Nice route with NiceNamedM () () as model, wrapping something and deriving /newtype/
-- GHC.Generic.
newtype NewtypeWrappedR_NiceNamedM a
  = NewtypeWrappedR_NiceNamedM a
  deriving newtype (GHC.Generic)

instance IsRoute (NewtypeWrappedR_NiceNamedM a) where
  type RouteModel (NewtypeWrappedR_NiceNamedM a) = NiceNamedM () ()
  routePrism = undefined
  routeUniverse = undefined

-- * Test cases

----------------------------------------
-- Subroute verification
----------------------------------------

-- routeSpec "subroutes should not have constructors with multiple fields"
--   (badRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      ]
--   |]
--   [r|
-- MultiRoute: too many arguments
--   |]

-----------------------------------------

-- routeSpec "'WithSubRoutes' list should not be shorter than number of route constructors"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ () ]
--      ]
--   |]
--   [r|
-- 'WithSubRoutes' is missing subroutes for:
-- 
--   '[ '[()]]
--   |]

-----------------------------------------

-- routeSpec "'WithSubRoutes' list should not be longer than number of route constructors"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ (), (), () ]
--      ]
--   |]
--   [r|
-- 'WithSubRoutes' has extra unnecessary types: 
-- 
--   '[()]
--   |]

------------------------------------------

-- routeSpec "constructors should either be empty or contain () when 'WithSubRoutes' specifies ()"
--   (niceRoute ''Int ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- A 'WithSubRoutes' entry is '()' instead of the expected: 
-- '[Int]
--   |]

-------------------------------------------

-- routeSpec "subroute types that are nonisomorphic to what is specified in 'WithSubRoutes' should be illegal"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ (), Bool ]
--      ]
--   |]
--   [r|
-- A 'WithSubRoutes' type:
-- 
--   Bool
-- 
-- is not isomorphic to the corresponding route constructor type:
-- 
--   '[()]
-- 
--   |]

-------------------------------------------

-- routeSpec "subroute types that are the same as what is specified in 'WithSubRoutes' should typecheck"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
--   |]

-------------------------------------------

-- routeSpec "subroute types that are an unwrapped representation of what is specified in 'WithSubRoutes' should typecheck | ( empty constructor <-> () ) special case"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ (), PlainR_NiceNamedM ]
--      ]
--   |]
--   [r|
--   |]

-------------------------------------------

-- routeSpec "subroute types that are an unwrapped representation of what is specified in 'WithSubRoutes' should typecheck | (wrapper deriving /newtype/ GHC.Generic) case"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ (), NewtypeWrappedR_NiceNamedM () ]
--      ]
--   |]
--   [r|
--   |]

-------------------------------------------

-- routeSpec "subroute types that are an unwrapped representation of what is specified in 'WithSubRoutes' should typecheck | (wrapper deriving /stock/ GHC.Generic) case"
--   (niceRoute ''() ''Slug)
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubRoutes '[ (), StockWrappedR_NiceNamedM Slug ]
--      ]
--   |]
--   [r|
--   |]

-------------------------------------------
-- Submodel verification
-------------------------------------------

-- routeSpec "submodel selectors should not be less than number of subroutes"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubModels '[ Proxy "niceNamed1" ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- 'WithSubModels' is missing submodel types: 
-- 
--   '[()]
--   |]

-------------------------------------------

-- routeSpec "submodel selectors should not outnumber number of subroutes"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubModels '[ Proxy "niceNamed1", Proxy "niceNamed2", Proxy "niceNamed2" ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- 'WithSubModels' has extra unnecessary types: 
-- 
--   '[Proxy "niceNamed2"]
--   |]

-------------------------------------------

-- routeSpec "submodel type selectors be able to reference the model itself if they are of the same type"
--   (niceRoute ''() ''PlainR_NiceNamedM)
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubModels '[ Proxy "niceNamed1", NiceNamedM () () ]
--      , WithSubRoutes '[ (), PlainR_NiceNamedM ]
--      ]
--   |]
--   [r|
--   |]

-------------------------------------------

-- routeSpec "submodel type selectors must, at a minimum, refer to a model field of a matching type"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceNamedM () ())
--      , WithSubModels '[ Proxy "niceNamed1", Bool ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- The 'WithSubModel' selector Bool of 'NiceNamedM
--                                        () ()' is not of expected type:
-- 
--   ()
--   |]

-----------------------------------------

-- routeSpec "submodel field name selectors on models with multiple constructors should be illegal"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (BadM () ())
--      , WithSubModels '[ Proxy "niceNamed1", () ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- Type rep field name lookup: multiple constructors
--   |]

-----------------------------------------


-- routeSpec "submodel field name selectors should be illegal on models with anonymous fields"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceAnonM () ())
--      , WithSubModels '[ Proxy "niceNamed1", Proxy "niceNamed2" ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- Type rep field name lookup: constructor has anonymous fields.
--   |]

-------------------------------------------

-- routeSpec "submodel position selectors on models with multiple constructors should be illegal"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (BadM () ())
--      , WithSubModels '[ Proxy 1, () ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- Type rep indexing: multiple constructors
--   |]

-----------------------------------------

-- routeSpec "submodel position selectors should not go above bounds"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceAnonM () ())
--      , WithSubModels '[ Proxy 1, Proxy 4 ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- Type rep indexing: out of bounds index 4
--   |]

-------------------------------------------

-- routeSpec "submodel position selectors should not go below bounds"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceAnonM () ())
--      , WithSubModels '[ Proxy 1, Proxy 0 ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- Type rep indexing: generic selector indexing starts at 1
--   |]

-------------------------------------------

-- routeSpec "submodel position selectors should not go below bounds"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceAnonM () ())
--      , WithSubModels '[ Proxy 1, Proxy 0 ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- Type rep indexing: generic selector indexing starts at 1
--   |]

-------------------------------------------

-- routeSpec "submodel position selectors should not go below bounds"
--   (niceRoute ''() ''())
--   [t|
--     '[ WithModel (NiceAnonM () ())
--      , WithSubModels '[ Proxy 1, Proxy 0 ]
--      , WithSubRoutes '[ (), () ]
--      ]
--   |]
--   [r|
-- Type rep indexing: generic selector indexing starts at 1
--   |]
