module Main where

import Ema.DynamicSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Ema.DynamicSpec.spec
