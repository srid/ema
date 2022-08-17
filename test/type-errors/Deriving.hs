{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Deriving where

import Data.Proxy (Proxy)
import Data.Text (Text)
import Ema.Route.Class
import Ema.Route.Generic
import Ema.Route.Generic.TH
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Text.RawString.QQ (r)

#define ENABLE_SPEC

----------------------------------------
-- Subroute verification
----------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data BadRoute = BR_1 Int String | BR_2 String

deriveGeneric ''BadRoute
-- Subroutes should not have constructors with multiple fields
-- Expect: MultiRoute: too many arguments
deriveIsRoute ''BadRoute [t|
    '[ WithModel (NiceNamedM () ())
     ]
  |]
#endif
#define ENABLE_SPEC

-----------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data R = R_1 () | R_2 ()
deriveGeneric ''R
-- WithSubRoutes' list should not be shorter than number of route constructors
-- Expect:
{-
'WithSubRoutes' is missing subroutes for:

  '[()]
-}
deriveIsRoute ''R [t|
  '[ WithSubRoutes '[ () ] ]
  |]
#endif
#define ENABLE_SPEC

-----------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data R = R_1 () | R_2 ()
deriveGeneric ''R
-- WithSubRoutes' list should not be longer than number of route constructors
-- Expect:
{-
'WithSubRoutes' has extra unnecessary types:

  '[Int]
-}
deriveIsRoute ''R [t|
  '[ WithSubRoutes '[ (), (), Int ] ]
  |]
#endif
#define ENABLE_SPEC

------------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data R = R_1 Int | R_2
deriveGeneric ''R
-- constructors should either be empty or contain () when 'WithSubRoutes' specifies ()
-- Expect:
{-
A 'WithSubRoutes' entry is '()' instead of the expected:
Int
-}
deriveIsRoute ''R [t|
  '[ WithSubRoutes '[ (), () ] ]
  |]
#endif
#define ENABLE_SPEC

-------------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data R = R_1 | R_2 
deriveGeneric ''R
-- subroute types that are nonisomorphic to what is specified in 'WithSubRoutes' should be illegal
-- Expect:
{-
Couldn't match representation of type ‘()’ with that of ‘Bool’
  arising from a use of ‘routePrism’
-}
deriveIsRoute ''R [t|
  ' [ WithSubRoutes '[ (), Bool] ]
  |]
#endif
#define ENABLE_SPEC

-------------------------------------------

-- subroute types that are the same as what is specified in 'WithSubRoutes' should typecheck"
data RSubIso = RSubIso_1 | RSubIso_2
deriveGeneric ''RSubIso
deriveIsRoute
  ''RSubIso
  [t|
    '[WithSubRoutes '[(), ()]]
    |]

-------------------------------------------

-- FIXME: This test is broken.
#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
routeSpec "subroute types that are an unwrapped representation of what is specified in 'WithSubRoutes' should typecheck | ( empty constructor <-> () ) special case"
  (niceRoute ''() ''())
  [t|
    '[ WithModel (NiceNamedM () ())
     , WithSubRoutes '[ (), PlainR_NiceNamedM ]
     ]
  |]
  [r|
  |]
#endif
#define ENABLE_SPEC

-------------------------------------------

-- Subroute types that are an unwrapped representation of what is specified in
-- 'WithSubRoutes' should typecheck | (wrapper deriving /newtype/ GHC.Generic)
-- case"
data RSubNewtype = RSubNewtype_1 | RSubNewtype_2
deriveGeneric ''RSubNewtype
newtype WrapNewtype a = WrapNewtype a
  deriving newtype (GHC.Generic, IsRoute)
deriveIsRoute
  ''RSubNewtype
  [t|
    '[ WithSubRoutes '[(), WrapNewtype ()]
     ]
    |]

-------------------------------------------
-- Submodel verification
-------------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data R = R1 | R2 
deriveGeneric ''R 
data M = M { m1 :: (), m2 :: () } deriving stock GHC.Generic
-- Submodel selectors should not be less than number of subroutes
-- Expect:
{-
'WithSubModels' is missing submodel types:
  '[()]
-}
deriveIsRoute ''R 
  [t|
    '[ WithModel M
     , WithSubModels '[ Proxy "m1" ]
     ]
  |]
#endif
#define ENABLE_SPEC

-------------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data R = R1 | R2 
deriveGeneric ''R 
data M = M { m1 :: (), m2 :: () }
  deriving stock GHC.Generic
-- submodel selectors should not outnumber number of subroutes
-- Expect:
{-
'WithSubModels' has extra unnecessary types:

  '[Proxy "m2"]
-}
deriveIsRoute ''R
  [t|
    '[ WithModel M
     , WithSubModels '[ Proxy "m1", Proxy "m2", Proxy "m2" ]
     ]
  |]
#endif
#define ENABLE_SPEC

-------------------------------------------
data RSubMSelf = RSubMSelf1 | RSubMSelf2 RSubMSelf
  deriving stock (GHC.Generic)
deriveGeneric ''RSubMSelf
data MSelf = MSelf {mself1 :: (), mself2 :: ()}
  deriving stock (GHC.Generic)

-- submodel type selectors be able to reference the model itself if they are of the same type
deriveIsRoute
  ''RSubMSelf
  [t|
    '[ WithModel MSelf
     , WithSubModels '[Proxy "mself1", MSelf]
     ]
    |]

-------------------------------------------

#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
data R = R1 | R2
deriveGeneric ''R
data M = M { m1 :: (), m2 :: () }
  deriving stock GHC.Generic
-- submodel type selectors must, at a minimum, refer to a model field of a matching type
-- Expect:
{-
The 'WithSubModel' selector Bool of 'M' is not of expected type:

  ()
-}
deriveIsRoute ''R
  [t|
    '[ WithModel M
     , WithSubModels '[ Proxy "m1", Bool ]
     ]
  |]
#endif
#define ENABLE_SPEC

-----------------------------------------

-- | Low priority
#undef ENABLE_SPEC
#ifdef ENABLE_SPEC
routeSpec "submodel field name selectors on models with multiple constructors should be illegal"
  (niceRoute ''() ''())
  [t|
    '[ WithModel (BadM () ())
     , WithSubModels '[ Proxy "niceNamed1", () ]
     , WithSubRoutes '[ (), () ]
     ]
  |]
  [r|
Type rep field name lookup: multiple constructors
  |]
#endif
#define ENABLE_SPEC
