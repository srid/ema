{-# LANGUAGE TemplateHaskell #-}

module Ema.Route.Generic.TH (
  deriveIsRoute
) where

import Ema.Route.Class (IsRoute)
import Language.Haskell.TH

{-| @deriveIsRoute route model subroutes@ derives 'HasSubRoutes', 'HasSubModels', and 'IsRoute' for the given @route@.

Subroutes are optionally supplied, but if they are then the length of the list must be the same as the number of
constructors in @route@.

TODO: Add TypeErrors to catch mismatched 'WithSubRoutes' list shapes at the generic deriving level?
-}
deriveIsRoute :: Name -> Name -> Maybe [Name] -> Q [Dec]
deriveIsRoute route model subroutes = do
  let instances = 
        [ "HasSubRoutes"
        , "HasSubModels"
        , "IsRoute"
        ]
  let opts = 
        toTyList $
          [ ConT (mkName "WithModel") `AppT` (ConT model) ]
            <> maybe [] (\s -> [ ConT (mkName "WithSubRoutes") `AppT` toTyList (ConT <$> s) ]) subroutes
  pure $ flip fmap instances $ \i ->
    StandaloneDerivD 
      (Just (ViaStrategy 
        (ConT (mkName "GenericRoute")
          `AppT` (ConT route)
          `AppT` opts)))
    []
    (ConT (mkName i) `AppT` ConT route)
  where
    toTyList (n:ns) = PromotedConsT `AppT` n `AppT` toTyList ns 
    toTyList []     = PromotedNilT
