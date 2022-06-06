{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Most trivial Ema program
module Ema.Example.Ex00_Hello where

import Ema
import Generics.SOP.TH (deriveGeneric)

data Route = Route_Index
  deriving stock (Show, Eq, Ord)
deriveGeneric ''Route
deriving anyclass instance IsRoute Route

instance EmaSite Route where
  siteOutput _enc _m Route_Index =
    Ema.AssetGenerated Ema.Html "<b>Hello</b>, Ema"

main :: IO ()
main = void $ Ema.runSite @Route ()
