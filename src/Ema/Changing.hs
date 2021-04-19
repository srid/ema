{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Changing where

-- A mutable variable with change notification
--
-- one subscriber only (because of TMVar)

-- TODO: Rename to something more accurate?
-- TODO: Support multiple observers (when supporting multiple websocket clients)
data Changing a = Changing
  { -- | A value that changes over time
    changingCurrent :: TMVar a,
    -- | To get notified of whenever the value changes.
    --
    -- Only one reader is supported.
    changingUpdated :: TMVar ()
  }

new :: MonadIO m => a -> m (Changing a)
new val = do
  Changing <$> newTMVarIO val <*> newEmptyTMVarIO

empty :: MonadIO m => m (Changing a)
empty =
  Changing <$> newEmptyTMVarIO <*> newEmptyTMVarIO

get :: MonadIO m => Changing a -> m a
get v =
  atomically $ readTMVar $ changingCurrent v

set :: MonadIO m => Changing a -> a -> m ()
set v val =
  atomically $ do
    void $ swapTMVar (changingCurrent v) val
    void $ tryPutTMVar (changingUpdated v) ()
