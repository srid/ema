{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Generate where

import Ema.Route (IsRoute, routeFile)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath

generate ::
  (Show route, IsRoute route) =>
  FilePath ->
  [route] ->
  (route -> LByteString) ->
  IO ()
generate dest routes render = do
  unlessM (doesDirectoryExist dest) $ do
    error "Destination does not exist"
  putStrLn $ "Writing " <> show (length routes) <> " routes"
  forM_ routes $ \r -> do
    let fp = dest </> routeFile r
        !s = render r
    createDirectoryIfMissing True (takeDirectory fp)
    putStrLn $ "W " <> fp
    writeFileLBS fp s
