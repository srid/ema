{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Generate where

import Ema.Route (IsRoute, routeFile)
import System.FilePath

generate ::
  (Show route, IsRoute route) =>
  FilePath ->
  [route] ->
  (route -> LByteString) ->
  IO ()
generate dest routes render = do
  putStrLn $ "Writing " <> show (length routes) <> " routes"
  forM_ routes $ \r -> do
    let fp = dest </> routeFile r
        !s = render r
    putStrLn $ "W " <> fp
    writeFileLBS fp s
