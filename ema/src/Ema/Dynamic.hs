module Ema.Dynamic (
  Dynamic (Dynamic),
  currentValue,
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

{- | Tee a 'Dynamic' so its latest value is readable out-of-band.

Returns @(reader, wrapped)@:

* @reader :: IO a@ yields the most recently pushed value, or the initial
  value before any update.
* @wrapped@ is a pass-through 'Dynamic' that must be used in place of the
  input. Its updater calls the input's updater and additionally stores
  each value for the reader.

'Dynamic' is push-only: the updater takes a send callback and runs
forever, producing values via that callback. That shape is a poor fit
for consumers that need synchronous pull access to the current value
(e.g. an HTTP handler running alongside a render loop). This helper
bridges the gap without forking a second producer.

Usage:

> (readModel, dyn') <- currentValue dyn
> race_
>   (runSiteWith siteConfig siteArg dyn')   -- consumes dyn'
>   (serveHttp $ \req -> readModel)         -- reads current model per request
-}
currentValue :: (MonadIO m) => Dynamic m a -> m (IO a, Dynamic m a)
currentValue (Dynamic (x0, xf)) = do
  ref <- liftIO $ newIORef x0
  pure
    ( readIORef ref
    , Dynamic
        ( x0
        , \send -> xf $ \x -> do
            liftIO $ writeIORef ref x
            send x
        )
    )

instance (MonadUnliftIO m, MonadLogger m) => Applicative (Dynamic m) where
  pure x = Dynamic (x, const pass)
  liftA2 f (Dynamic (x0, xf)) (Dynamic (y0, yf)) =
    Dynamic
      ( f x0 y0
      , \send -> do
          var <- newTVarIO (x0, y0)
          sendLock :: TMVar () <- newEmptyTMVarIO
          race_
            ( do
                xf $ \x -> do
                  atomically $ putTMVar sendLock ()
                  logDebugNS "ema.dyn.app" "left update"
                  send <=< atomically $ do
                    modifyTVar' var $ first (const x)
                    f x . snd <$> readTVar var
                  atomically $ takeTMVar sendLock
                logDebugNS "ema.dyn.app" "updater exited; keeping thread alive"
                threadDelay maxBound
            )
            ( do
                yf $ \y -> do
                  atomically $ putTMVar sendLock ()
                  logDebugNS "ema.dyn.app" "right update"
                  send <=< atomically $ do
                    modifyTVar' var $ second (const y)
                    (`f` y) . fst <$> readTVar var
                  atomically $ takeTMVar sendLock
                logDebugNS "ema.dyn.app" "updater exited; keeping thread alive"
                threadDelay maxBound
            )
      )
