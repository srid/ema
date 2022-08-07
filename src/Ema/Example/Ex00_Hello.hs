{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Most trivial Ema program
module Ema.Example.Ex00_Hello where

import Ema

-- Let's newtype the unit route, because we have only one page to generate.
newtype Route = Route ()
  deriving newtype
    (Show, Eq, Ord, Generic, IsRoute)

instance EmaSite Route where
  siteInput _ _ =
    pure $ pure ()
  siteOutput _ _ _ =
    pure $ Ema.AssetGenerated Ema.Html "<b>Hello</b>, Ema"

main :: IO ()
main = void $ Ema.runSite @Route ()
