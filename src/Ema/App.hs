{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.App
  ( runEma,
    runEmaPure,
  )
where

import Data.LVar (LVar)
import qualified Data.LVar as LVar
import Ema.Route (IsRoute (..))
import qualified Ema.Server as Server
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)

-- | Pure version of @runEma@ (i.e with no model).
--
-- Due to purity, there is no impure state, and thus no time-varying model. The
-- render function consequently takes only the route as argument.
runEmaPure ::
  forall route.
  (IsRoute route, Show route) =>
  -- | How to render a route
  (route -> LByteString) ->
  IO ()
runEmaPure render = do
  emptyModel <- LVar.empty
  runEma emptyModel (const render)

-- | Run Ema live server
--
-- Continually observe the world, with a concomitant incremental update of the
-- model so as to hot-reload the browser view.
runEma ::
  forall model route.
  (Show route, IsRoute route) =>
  -- | Your site model
  LVar model ->
  -- | Your site render function
  (model -> route -> LByteString) ->
  IO ()
runEma model render = do
  -- TODO: Use a logging library, in place of managing buffering and using putStrLn
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Launching Ema at http://localhost:8000"
  Server.runServerWithWebSocketHotReload model render
