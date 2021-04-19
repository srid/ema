{-# LANGUAGE TypeApplications #-}

-- | A very simple site with routes, but based on dynamically changing date
-- (current time, in this example)
module Ema.Example.SimpleSite where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TVar (swapTVar)
import Data.List ((!!))
import qualified Data.Text as T
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
  )
import Ema.App (Changing (Changing), Ema (Ema), runEma)
import qualified Ema.Layout as Layout
import Ema.Route (IsRoute (..), Slug (unSlug))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  | Person Text
  deriving (Show)

instance IsRoute Route where
  toSlug = \case
    Index -> mempty
    Person name -> one $ fromString . toString $ T.replace " " "_" name
  fromSlug = \case
    [] -> Just Index
    [person] -> Just $ Person (T.replace "_" " " $ unSlug person)
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
          H.header ! A.class_ "text-4xl font-bold border-b-1" $ "Simple Site!"
          case r of
            Index -> do
              H.p ! A.style "color: red; text-3xl" $ "Checkout some profiles:"
              forM_ ["Srid Ratna", "ema", "Great India"] $ \person ->
                H.li $
                  routeElem (Person person) $
                    H.toMarkup person
            Person name -> do
              H.header ! A.class_ "text-2xl" $ H.toMarkup $ "Profile of " <> name
              H.div $ routeElem Index "Go to Home"
          H.footer ! A.class_ "border-t-1 p-2 text-center" $ do
            "The current time is: "
            H.pre ! A.class_ "text-4xl" $ do
              let epoch = fromMaybe 0 . readMaybe @Int $ formatTime defaultTimeLocale "%s" now
                  colors = ["green", "purple", "red", "blue"]
                  color = colors !! mod epoch (length colors)
                  cls = "text-" <> color <> "-500"
              H.span ! A.class_ cls $ H.toMarkup $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" now
    routeElem r w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r $ w
    routeHref r =
      A.href (fromString . toString $ routeUrl r)
