{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Generate where

import Ema.Class
import Ema.Route (routeFile)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))

generate ::
  forall model route.
  Ema model route =>
  FilePath ->
  model ->
  (model -> route -> LByteString) ->
  IO ()
generate dest model render = do
  unlessM (doesDirectoryExist dest) $ do
    error "Destination does not exist"
  let routes = staticRoutes model
  putStrLn $ "Writing " <> show (length routes) <> " routes"
  forM_ routes $ \r -> do
    let fp = dest </> routeFile @model r
        !s = render model r
    createDirectoryIfMissing True (takeDirectory fp)
    putStrLn $ "W " <> fp
    writeFileLBS fp s
