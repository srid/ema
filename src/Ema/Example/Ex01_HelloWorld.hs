-- | The simplest Ema site possible.
--
-- A site with one route (index) that displays content generated from pure
-- values.
module Ema.Example.Ex01_HelloWorld where

import Ema (runEmaPure)

main :: IO ()
main = do
  let speaker :: Text = "Ema"
  runEmaPure $ \_ iso ->
    encodeUtf8 $ "<b>Hello</b>, from " <> speaker
