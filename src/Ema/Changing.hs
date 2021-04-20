{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Changing where

-- A mutable variable with change notification
-- TODO: Rename to something more accurate?
import qualified Data.Map.Strict as Map

data Changing a = Changing
  { -- | A value that changes over time
    changingCurrent :: TMVar a,
    -- | Subscribers listening on changes to the value
    changingSubscribers :: TMVar (Map Int (TMVar ()))
  }

new :: MonadIO m => a -> m (Changing a)
new val = do
  Changing <$> newTMVarIO val <*> newTMVarIO mempty

empty :: MonadIO m => m (Changing a)
empty =
  Changing <$> newEmptyTMVarIO <*> newTMVarIO mempty

get :: MonadIO m => Changing a -> m a
get v =
  atomically $ readTMVar $ changingCurrent v

set :: MonadIO m => Changing a -> a -> m ()
set v val = do
  n <- atomically $ do
    void $ swapTMVar (changingCurrent v) val
    publish v
  when (n > 0) $
    putStrLn $ "pub: sent to " <> show n <> " subscribers"

publish :: Changing a -> STM Int
publish v = do
  subs <- readTMVar $ changingSubscribers v
  forM_ (Map.elems subs) $ \subVar -> do
    tryPutTMVar subVar ()
  pure $ Map.size subs

-- | Subscribes to new values as they are set.
subscribe :: MonadIO m => Changing a -> (Int -> a -> IO b) -> m (Int, IO ())
subscribe v f = do
  putStrLn "sub"
  (idx, notify) <- atomically $ do
    subs <- readTMVar $ changingSubscribers v
    let nextIdx = maybe 0 (succ . fst) $ Map.lookupMax subs
    notify <- newEmptyTMVar
    void $ swapTMVar (changingSubscribers v) $ Map.insert nextIdx notify subs
    pure (nextIdx, notify)
  putStrLn $ "sub[" <> show idx <> "]: created notify"
  pure
    ( idx,
      void $
        forever $ do
          val :: a <- atomically $ do
            takeTMVar notify
            readTMVar (changingCurrent v)
          putStrLn $ "sub[" <> show idx <> "]: calling f"
          liftIO $ f idx val
    )

unsubscribe :: MonadIO m => Changing a -> Int -> m ()
unsubscribe v subId = do
  putStrLn $ "unsub - " <> show subId
  atomically $ do
    subs <- readTMVar $ changingSubscribers v
    whenJust (Map.lookup subId subs) $ \_sub -> do
      void $ swapTMVar (changingSubscribers v) $ Map.delete subId subs
