{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | @LVar@ is like @Control.Concurrent.STM.TVar@ but with a capability for
-- listening to its changes.
module Data.LVar
  ( -- * Types
    LVar,
    ListenerId,

    -- * Creating a LVar
    new,
    empty,

    -- * Modifying a LVar
    get,
    set,
    modify,

    -- * Listening to a LVar
    listen,
    ignore,
  )
where

import qualified Data.Map.Strict as Map
import Prelude hiding (empty, get, modify)

-- A mutable variable, changes (@set@, @modify@) to which can be listened
-- (@listen@, @ignore@) to from multiple threads.
data LVar a = LVar
  { -- | A value that changes over time
    lvarCurrent :: TMVar a,
    -- | Subscribers listening on changes to the value
    lvarListeners :: TMVar (Map ListenerId (TMVar ()))
  }

type ListenerId = Int

-- | Create a new @LVar@ with the given initial value
new :: forall a m. MonadIO m => a -> m (LVar a)
new val = do
  LVar <$> newTMVarIO val <*> newTMVarIO mempty

-- | Like @new@, but there is no initial value. A @get@ will block until an
-- initial value is set using @set@ or @modify@
empty :: MonadIO m => m (LVar a)
empty =
  LVar <$> newEmptyTMVarIO <*> newTMVarIO mempty

-- | Get the value of the @LVar@
get :: MonadIO m => LVar a -> m a
get v =
  atomically $ readTMVar $ lvarCurrent v

-- | Set the @LVar@ value; listeners from @listen@ are automatically notifed.
set :: MonadIO m => LVar a -> a -> m ()
set v = modify v . const

-- | Modify the @LVar@ value; listeners from @listen@ are automatically
-- notified.
--
-- Returns the number of listeners notified.
modify :: MonadIO m => LVar a -> (a -> a) -> m ()
modify v f = do
  atomically $ do
    curr <- readTMVar (lvarCurrent v)
    void $ swapTMVar (lvarCurrent v) (f curr)
    notifyListeners v
  where
    notifyListeners :: LVar a -> STM ()
    notifyListeners v' = do
      subs <- readTMVar $ lvarListeners v'
      forM_ (Map.elems subs) $ \subVar -> do
        tryPutTMVar subVar ()

-- | Listen to changes to the @LVar@, as they are set by @set@ or @modify@
--
-- Returns a @ListenerId@ that can be used to stop listening later (via
-- @ignore@), as well as an IO action that starts the listener. You must run the
-- IO action to actually begin the listening process.
listen ::
  MonadIO m =>
  LVar a ->
  (ListenerId -> a -> IO b) ->
  m (ListenerId, IO ())
listen v f = do
  (idx, notify) <- atomically $ do
    subs <- readTMVar $ lvarListeners v
    let nextIdx = maybe 1 (succ . fst) $ Map.lookupMax subs
    notify <- newEmptyTMVar
    void $ swapTMVar (lvarListeners v) $ Map.insert nextIdx notify subs
    pure (nextIdx, notify)
  let runListener = do
        mval :: Maybe a <- atomically $ do
          isListening v idx >>= \case
            False ->
              -- Stop listening, because @ignore@ had just removed this
              -- listener.
              pure Nothing
            True -> do
              takeTMVar notify
              Just <$> readTMVar (lvarCurrent v)
        whenJust mval $ \val -> do
          liftIO $ void $ f idx val
          runListener
  pure (idx, runListener)
  where
    isListening :: LVar a -> ListenerId -> STM Bool
    isListening v' lId = do
      Map.member lId <$> readTMVar (lvarListeners v')

-- | Stop listening to the @LVar@
ignore :: MonadIO m => LVar a -> ListenerId -> m ()
ignore v lId = do
  atomically $ do
    subs <- readTMVar $ lvarListeners v
    whenJust (Map.lookup lId subs) $ \_sub -> do
      void $ swapTMVar (lvarListeners v) $ Map.delete lId subs
