{-# LANGUAGE TypeApplications #-}

-- | A very simple site with routes, but based on dynamically changing values
-- (current time, in this example)
module Ema.Example.SimpleSite where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TVar (swapTVar)
import Data.List ((!!))
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
  )
import Ema.App (Changing (Changing), Ema (Ema), runEma)
import qualified Ema.Layout as Layout
import Ema.Route (IsRoute (..))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  | About
  deriving (Show)

instance IsRoute Route where
  toSlug = \case
    Index -> mempty
    About -> one "about"
  fromSlug = \case
    [] -> Just Index
    ["about"] -> Just About
    _ -> Nothing

timeC :: IO (Changing UTCTime, IO ())
timeC = do
  var <- newTVarIO =<< getCurrentTime
  ch <- newEmptyTMVarIO
  let run = void $
        forever $ do
          now <- getCurrentTime
          atomically $ do
            void $ swapTVar var now
            void $ tryPutTMVar ch ()
          threadDelay $ 1 * 1000000
  pure (Changing var ch, run)

main :: IO ()
main = do
  (model, runTimeC) <- timeC
  race_ runTimeC (runEma $ Ema model render)
  where
    render now r =
      Layout.tailwindSite (H.title "Simple Site") $
        H.div ! A.class_ "container mx-auto" $ do
          H.header ! A.class_ "text-4xl font-bold border-b-1" $ "Simple Site"
          case r of
            Index -> do
              routeElem About "About this site"
            About -> do
              H.p "Just a simple site showing time"
          H.div ! A.class_ "border-t-1 p-2 text-center" $ do
            "The current time is: "
            H.pre ! A.class_ "text-4xl" $ do
              H.span ! A.class_ ("text-" <> randomColor now <> "-500") $
                H.toMarkup $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" now
    routeElem r w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r $ w
    routeHref r =
      A.href (fromString . toString $ routeUrl r)
    randomColor t =
      let epochSecs = fromMaybe 0 . readMaybe @Int $ formatTime defaultTimeLocale "%s" t
          colors = ["green", "purple", "red", "blue"]
       in colors !! mod epochSecs (length colors)