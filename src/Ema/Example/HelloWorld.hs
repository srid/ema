{-# LANGUAGE TypeApplications #-}

-- | The simplest Ema site possible.
--
-- A site with one route (index) that displays content generated from pure
-- values.
module Ema.Example.HelloWorld where

import Ema.App (runEmaPure)

main :: IO ()
main = do
  let name :: Text = "Srid"
  runEmaPure $ \() ->
    encodeUtf8 $ "Hello, " <> name
