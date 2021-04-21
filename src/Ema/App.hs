{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.App
  ( runEma,
    runEmaPure,
    runEmaWith,
  )
where

import Control.Concurrent.Async (race_)
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import Ema.Route (IsRoute (..))
import qualified Ema.Server as Server
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)

-- | Pure version of @runEmaWith@ (i.e with no model).
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
  runEmaWith emptyModel (const render)

-- | Convenient version of @runEmaWith@ that takes initial model and an update
-- function. You typically want to use this.
--
-- It uses @race_@ to properly clean up the update action when the ema thread
-- exits, and vice-versa.
runEma ::
  forall model route.
  (Show route, IsRoute route) =>
  -- | The initial model loaded during program start
  model ->
  -- | A long-running IO action that will update the @model@ @LVar@ over time.
  (LVar model -> IO ()) ->
  (model -> route -> LByteString) ->
  IO ()
runEma model0 updateModel render = do
  model <- LVar.new model0
  race_
    (runEmaWith model render)
    (updateModel model)

-- | Run Ema live dev server
runEmaWith ::
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
runEmaWith model render = do
  -- TODO: Use a logging library, in place of managing buffering and using putStrLn
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let port = 8000
  putStrLn $ "Launching Ema at http://localhost:" <> show port
  Server.runServerWithWebSocketHotReload port model render
