{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.App where

import Ema.Changing
  ( Changing,
  )
import qualified Ema.Changing as Changing
import Ema.Route (IsRoute (..))
import qualified Ema.Server as Server
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)

data Ema s r = Ema
  { -- | The (ever-changing) state of the app
    emaModel :: Changing s,
    -- | HTML view function over app state and app route
    emaRender :: s -> r -> LByteString
  }

-- | Pure version of @runEma@ (i.e with no model).
--
-- Due to purity, there is no impure state, and thus no time-varying model. The
-- render function consequently takes only the route as argument.
runEmaPure ::
  forall r.
  (IsRoute r, Show r) =>
  -- | How to render a route
  (r -> LByteString) ->
  IO ()
runEmaPure render = do
  emptyModel <- Changing.empty
  runEma $ Ema emptyModel (const render)

-- | Run Ema live server
--
-- Continually observe the world, with a concomitant incremental update of the
-- model so as to hot-reload the browser view.
runEma ::
  forall model route.
  (Show route, IsRoute route) =>
  Ema model route ->
  IO ()
runEma ema = do
  -- TODO: Use a logging library, in place of managing buffering and using putStrLn
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Launching Ema at http://localhost:3000"
  Server.runServerWithWebSocketHotReload (emaModel ema) (emaRender ema)
