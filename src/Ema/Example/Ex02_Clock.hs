{-# LANGUAGE TypeApplications #-}

-- | A very simple site with routes, but based on dynamically changing values
--
-- The current time is computed in the server every second, and the resultant
-- generated HTML is automatically updated on the browser. This is only a demo;
-- usually we render HTML based on files on disk or something accessible outside
-- of the browser. More advanced examples will demonstrate that.
module Ema.Example.Ex02_Clock where

import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar
import Data.List ((!!))
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
  )
import Ema.App (runEma)
import qualified Ema.Helper.Tailwind as Tailwind
import Ema.Route (IsRoute (..), routeUrl)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  | OnlyTime
  deriving (Show)

instance IsRoute Route where
  toSlug = \case
    Index -> mempty
    OnlyTime -> one "time"
  fromSlug = \case
    [] -> Just Index
    ["time"] -> Just OnlyTime
    _ -> Nothing

changeTime :: LVar.LVar UTCTime -> IO ()
changeTime model = do
  forever $ do
    threadDelay $ 1 * 1000000
    LVar.set model =<< getCurrentTime

main :: IO ()
main = do
  runEma (const [Index, OnlyTime]) render $ \model -> do
    LVar.set model =<< getCurrentTime
    changeTime model

render :: UTCTime -> Route -> LByteString
render now r =
  Tailwind.layout (H.title "Clock") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "border-t-1 p-2 text-center" $ do
        "The current time is: "
        H.pre ! A.class_ "text-6xl font-bold mt-2" $ do
          H.span ! A.class_ ("text-" <> randomColor now <> "-500") $ do
            let fmt = case r of
                  Index -> "%Y/%m/%d %H:%M:%S"
                  OnlyTime -> "%H:%M:%S"
            H.toMarkup $ formatTime defaultTimeLocale fmt now
      H.div ! A.class_ "mt-4 text-center" $ do
        case r of
          Index -> do
            routeElem OnlyTime "Hide day?"
          OnlyTime -> do
            routeElem Index "Show day?"
  where
    routeElem r' w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ routeUrl r')
    randomColor t =
      let epochSecs = fromMaybe 0 . readMaybe @Int $ formatTime defaultTimeLocale "%s" t
          colors = ["green", "gray", "purple", "red", "blue", "yellow", "black", "pink"]
       in colors !! mod epochSecs (length colors)
