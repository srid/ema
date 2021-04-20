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
-- render function consequently takes only the @route@ as argument.
runEmaPure ::
  forall route.
  (IsRoute route, Show route) =>
  -- | How to render a route
  (route -> LByteString) ->
  IO ()
runEmaPure render = do
  emptyModel <- LVar.empty
  runEma emptyModel (const render)

-- | Run Ema live dev server
runEma ::
  forall model route.
  (Show route, IsRoute route) =>
  -- | Your site model type, as a @LVar@ in order to support modifications over
  -- time (for hot-reload).
  --
  -- Use @Data.LVar.new@ to create it, and then -- over time -- @Data.LVar.set@
  -- or @Data.LVar.modify@ to modify it. Ema will automatically hot-reload your
  -- site as this model data changes.
  LVar model ->
  -- | Your site render function. Takes the current @model@ value, and the page
  -- @route@ type as arguments. It must return the raw HTML to render to browser
  -- or generate on disk.
  (model -> route -> LByteString) ->
  IO ()
runEma model render = do
  -- TODO: Use a logging library, in place of managing buffering and using putStrLn
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let port = 8000
  putStrLn $ "Launching Ema at http://localhost:" <> show port
  Server.runServerWithWebSocketHotReload port model render
