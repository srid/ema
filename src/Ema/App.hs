{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.App
  ( runEma,
    runEmaPure,
    runEmaWith,
    Ema (..),
  )
where

import Control.Concurrent.Async (race_)
import Data.Default
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import Ema.CLI
import qualified Ema.CLI as CLI
import Ema.Class
import qualified Ema.Generate as Generate
import Ema.Route.UrlStrategy
import qualified Ema.Server as Server
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import System.Environment (lookupEnv)

-- | Pure version of @runEmaWith@ (i.e with no model).
--
-- Due to purity, there is no impure state, and thus no time-varying model. The
-- render function consequently takes only the @route@ as argument.
runEmaPure ::
  -- | How to render a route
  (Route () -> LByteString) ->
  IO ()
runEmaPure render = do
  action <- CLI.cliAction
  emptyModel <- LVar.empty
  runEmaWith @() action emptyModel (const render)

-- | Convenient version of @runEmaWith@ that takes initial model and an update
-- function. You typically want to use this.
--
-- It uses @race_@ to properly clean up the update action when the ema thread
-- exits, and vice-versa.
runEma ::
  forall model.
  (Ema model, Show (Route model)) =>
  -- | A long-running IO action that will update the @model@ @LVar@ over time.
  -- This IO action must set the initial model value in the very beginning.
  (model -> Route model -> LByteString) ->
  (LVar model -> IO ()) ->
  IO ()
runEma render runModel = do
  action <- CLI.cliAction
  model <- LVar.empty
  race_
    (runModel model)
    (runEmaWith action model render)

-- | Run Ema live dev server
runEmaWith ::
  forall model.
  (Ema model, Show (Route model)) =>
  Action ->
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
  (model -> Route model -> LByteString) ->
  IO ()
runEmaWith action model render = do
  -- TODO: Use a logging library, in place of managing buffering and using putStrLn
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Waiting for initial site model ..."
  putStrLn "  stuck here? set a model value using `LVar.set`"
  case action of
    Generate dest -> do
      val <- LVar.get model
      Generate.generate dest val render
    Run -> do
      void $ LVar.get model
      port <- fromMaybe 8000 . (readMaybe @Int =<<) <$> lookupEnv "PORT"
      putStrLn $ "Launching Ema at http://localhost:" <> show port
      Server.runServerWithWebSocketHotReload port model render
