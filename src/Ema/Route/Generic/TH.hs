{-# LANGUAGE TemplateHaskell #-}

module Ema.Route.Generic.TH (
  deriveIsRoute
) where

import Ema.Route.Class (IsRoute)
import Ema.Route.Generic (GenericRoute, HasSubRoutes, HasSubModels)
import Language.Haskell.TH

{-| @deriveIsRoute route model subroutes@ derives 'HasSubRoutes', 'HasSubModels', and 'IsRoute' for the given @route@.

Subroutes are optionally supplied, but if they are then the length of the list must be the same as the number of
constructors in @route@.

TODO: Add TypeErrors to catch mismatched 'WithSubRoutes' list shapes at the generic deriving level?
-}
deriveIsRoute :: Name -> TypeQ -> Q [Dec]
deriveIsRoute route opts = do
  opts' <- opts
  let instances = 
        [ ''HasSubRoutes
        , ''HasSubModels
        , ''IsRoute
        ]
  pure $ flip fmap instances $ \i ->
    StandaloneDerivD 
      (Just (ViaStrategy 
        (ConT ''GenericRoute
          `AppT` (ConT route)
          `AppT` opts')))
    []
    (ConT i `AppT` ConT route)
