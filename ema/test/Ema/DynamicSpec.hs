module Ema.DynamicSpec where

import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Ema.Dynamic (Dynamic (Dynamic), currentValue)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import UnliftIO.Async (withAsync)

spec :: Spec
spec = do
  describe "currentValue" $ do
    it "returns initial value before any update" $ do
      (readNow, _) <- currentValue (Dynamic (42 :: Int, \_ -> pass))
      readNow `shouldReturn` 42
    it "tracks the latest value pushed through the wrapped Dynamic" $ do
      (readNow, Dynamic (_, xf)) <-
        currentValue (Dynamic (0 :: Int, \send -> mapM_ send [1, 2, 3]))
      xf $ \_ -> pass
      readNow `shouldReturn` 3

  describe "Functor" $ do
    it "fmap transforms the initial value" $ do
      let Dynamic (x0, _) =
            fmap (+ 1) (Dynamic (5 :: Int, \_ -> pass) :: Dynamic IO Int)
      x0 `shouldBe` 6
    it "fmap transforms each value pushed through the updater" $ do
      sent <- newIORef ([] :: [Int])
      let Dynamic (_, xf) =
            fmap
              (* 10)
              (Dynamic (0 :: Int, \send -> mapM_ send [1, 2, 3]) :: Dynamic IO Int)
      xf $ \x -> modifyIORef' sent (x :)
      reverse <$> readIORef sent `shouldReturn` [10, 20, 30]

  describe "Applicative" $ do
    it "pure has a no-op updater" $ runNoLoggingT $ do
      let Dynamic (x0, xf) = pure (7 :: Int) :: Dynamic (NoLoggingT IO) Int
      sent <- liftIO $ newIORef ([] :: [Int])
      xf $ \x -> liftIO $ modifyIORef' sent (x :)
      liftIO $ do
        x0 `shouldBe` 7
        readIORef sent `shouldReturn` []

    it "liftA2 combines initial values and tracks updates from both sides" $
      runNoLoggingT $ do
        gateA <- newEmptyMVar
        gateB <- newEmptyMVar
        doneA <- newEmptyMVar
        doneB <- newEmptyMVar
        let dA, dB :: Dynamic (NoLoggingT IO) Int
            dA =
              Dynamic
                ( 1
                , \send -> do
                    takeMVar gateA
                    send 10
                    putMVar doneA ()
                )
            dB =
              Dynamic
                ( 2
                , \send -> do
                    takeMVar gateB
                    send 20
                    putMVar doneB ()
                )
        (readNow, Dynamic (init0, xf)) <- currentValue ((,) <$> dA <*> dB)
        liftIO $ do
          init0 `shouldBe` (1, 2)
          readNow `shouldReturn` (1, 2)
        withAsync (xf $ \_ -> pass) $ \_ -> do
          putMVar gateA ()
          takeMVar doneA
          liftIO $ readNow `shouldReturn` (10, 2)
          putMVar gateB ()
          takeMVar doneB
          liftIO $ readNow `shouldReturn` (10, 20)
