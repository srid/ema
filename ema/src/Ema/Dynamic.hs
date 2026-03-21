module Ema.Dynamic (
  Dynamic (Dynamic),
) where

import Control.Monad.Logger (MonadLogger, logDebugNS)
import UnliftIO (MonadUnliftIO, race_)
import UnliftIO.Concurrent (threadDelay)

{- | A time-varying value of type `a`, changing under monad `m`.

  To create a `Dynamic`, supply the initial value along with a function that
  forever updates it using the given monadic update function.

 `Dynamic`'s can be composed using `Applicative`.
-}
newtype Dynamic m a
  = Dynamic
      ( -- Initial value
        a
      , -- Set a new value
        (a -> m ()) -> m ()
      )

instance Functor (Dynamic m) where
  fmap f (Dynamic (x0, xf)) =
    Dynamic
      ( f x0
      , \send -> xf $ send . f
      )

instance (MonadUnliftIO m, MonadLogger m) => Applicative (Dynamic m) where
  pure x = Dynamic (x, const pass)
  liftA2 f (Dynamic (x0, xf)) (Dynamic (y0, yf)) =
    Dynamic
      ( f x0 y0
      , \send -> do
          var <- newTVarIO (x0, y0)
          sendLock :: TMVar () <- newEmptyTMVarIO
          let
            -- Serialize updates through the lock, applying the STM action and
            -- sending the result. This prevents interleaved updates from
            -- producing inconsistent views.
            lockedUpdate label stmAction = do
              atomically $ putTMVar sendLock ()
              logDebugNS "ema.dyn.app" label
              send =<< atomically stmAction
              atomically $ takeTMVar sendLock
            -- Keep a thread alive after its updater exits, so race_ doesn't
            -- cancel the other side.
            keepAlive updater = do
              updater
              logDebugNS "ema.dyn.app" "updater exited; keeping thread alive"
              threadDelay maxBound
          race_
            ( keepAlive $ xf $ \x ->
                lockedUpdate "left update" $ do
                  modifyTVar' var $ first (const x)
                  f x . snd <$> readTVar var
            )
            ( keepAlive $ yf $ \y ->
                lockedUpdate "right update" $ do
                  modifyTVar' var $ second (const y)
                  (`f` y) . fst <$> readTVar var
            )
      )
