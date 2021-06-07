{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A very simple site with routes, but based on dynamically changing values
--
-- The current time is computed in the server every second, and the resultant
-- generated HTML is automatically updated on the browser. This is only a demo;
-- usually we render HTML based on files on disk or something accessible outside
-- of the browser. More advanced examples will demonstrate that.
module Ema.Example.Ex03_Clock where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (logDebugNS)
import qualified Data.LVar as LVar
import Data.List ((!!))
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  | OnlyTime
  deriving (Show, Enum, Bounded)

instance Ema UTCTime Route where
  encodeRoute _time = \case
    Index -> "index.html"
    OnlyTime -> "time.html"
  decodeRoute _time = \case
    "index.html" -> Just Index
    "time.html" -> Just OnlyTime
    _ -> Nothing

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \_act model ->
    forever $ do
      logDebugNS "ex:clock" "Refreshing time"
      LVar.set model =<< liftIO getCurrentTime
      liftIO $ threadDelay 1000000

render :: Ema.CLI.Action -> UTCTime -> Route -> LByteString
render emaAction now r =
  Tailwind.layout emaAction (H.title "Clock" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        case r of
          Index ->
            "The current date & time is: "
          OnlyTime ->
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
      A.href (fromString . toString $ Ema.routeUrl now r')
    randomColor t =
      let epochSecs = fromMaybe 0 . readMaybe @Int $ formatTime defaultTimeLocale "%s" t
          colors = ["green", "gray", "purple", "red", "blue", "yellow", "black", "pink"]
       in colors !! mod epochSecs (length colors)
