{-# LANGUAGE TypeApplications #-}

-- | The simplest Ema site possible.
--
-- A site with one route (index) that displays content generated from pure
-- values.
module Ema.Example.Ex01_HelloWorld where

import Ema.App (runEmaPure)

main :: IO ()
main = do
  let name :: Text = "Srid"
  runEmaPure (one ()) $ \() ->
    encodeUtf8 $ "Hello, " <> name
