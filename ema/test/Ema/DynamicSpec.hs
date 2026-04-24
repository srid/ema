module Ema.DynamicSpec where

import Ema.Dynamic (Dynamic (Dynamic), currentValue)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = describe "currentValue" $ do
  it "returns initial value before any update" $ do
    (readNow, _) <- currentValue (Dynamic (42 :: Int, \_ -> pass))
    readNow `shouldReturn` 42
  it "tracks the latest value pushed through the wrapped Dynamic" $ do
    (readNow, Dynamic (_, xf)) <-
      currentValue (Dynamic (0 :: Int, \send -> mapM_ send [1, 2, 3]))
    xf $ \_ -> pass
    readNow `shouldReturn` 3
