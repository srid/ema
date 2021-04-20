{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

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

-- A mutable variable with change notification
-- TODO: Rename to something more accurate?
data LVar a = LVar
  { -- | A value that changes over time
    lvarCurrent :: TMVar a,
    -- | Subscribers listening on changes to the value
    lvarListeners :: TMVar (Map ListenerId (TMVar ()))
  }

type ListenerId = Int

new :: forall a m. MonadIO m => a -> m (LVar a)
new val = do
  LVar <$> newTMVarIO val <*> newTMVarIO mempty

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

-- | Modify the @LVar@ value
modify :: MonadIO m => LVar a -> (a -> a) -> m ()
modify v f = do
  n <- atomically $ do
    curr <- readTMVar (lvarCurrent v)
    void $ swapTMVar (lvarCurrent v) (f curr)
    notifyListeners v
  when (n > 0) $
    putStrLn $ "pub: published; " <> show n <> " subscribers listening"
  where
    notifyListeners :: LVar a -> STM Int
    notifyListeners v' = do
      subs <- readTMVar $ lvarListeners v'
      forM_ (Map.elems subs) $ \subVar -> do
        tryPutTMVar subVar ()
      pure $ Map.size subs

-- | Listen to changes to the @LVar@, as they are set by @set@ or @modify@
--
-- Returns a @ListenerId@ that can be used to stop listening later (via
-- @ignore@), as well as an IO action that starts the listener.
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
  let runSubscription =
        forever $ do
          val :: a <- atomically $ do
            takeTMVar notify
            readTMVar (lvarCurrent v)
          putStrLn $ "sub[" <> show idx <> "]: sending"
          liftIO $ void $ f idx val
  pure (idx, runSubscription)

-- | Stop listening to the @LVar@
ignore :: MonadIO m => LVar a -> ListenerId -> m ()
ignore v subId = do
  putStrLn $ "unsub - " <> show subId
  atomically $ do
    subs <- readTMVar $ lvarListeners v
    whenJust (Map.lookup subId subs) $ \_sub -> do
      void $ swapTMVar (lvarListeners v) $ Map.delete subId subs
