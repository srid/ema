-- | A very simple site with routes, but based on dynamically changing values
--
-- The current time is computed in the server every second, and the resultant
-- generated HTML is automatically updated on the browser. This is only a demo;
-- usually we render HTML based on files on disk or something accessible outside
-- of the browser. More advanced examples will demonstrate that.
module Ema.Example.Ex03_Clock where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (logInfoNS)
import Data.LVar qualified as LVar
import Data.List ((!!))
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Ema
import Ema qualified
import Ema.Example.Common (tailwindLayout)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Index
  | OnlyTime
  deriving stock (Show, Eq, Enum, Bounded)

routeEncoder :: RouteEncoder a Route
routeEncoder =
  (enc, dec, all_)
  where
    enc _time = \case
      Index -> "index.html"
      OnlyTime -> "time.html"
    dec _time = \case
      "index.html" -> Just Index
      "time.html" -> Just OnlyTime
      _ -> Nothing
    all_ _ = Ema.defaultEnum @Route

site :: Site UTCTime Route
site =
  Site
    { siteName = "Ex03",
      siteRender = \_ enc m r ->
        Ema.AssetGenerated Ema.Html $ render enc m r,
      siteModelPatcher = \_ startModel -> do
        t0 <- liftIO getCurrentTime
        startModel t0 $ \lvar -> do
          logInfoNS "Ex03" "Starting clock..."
          forever $ do
            liftIO $ threadDelay 1000000
            t <- liftIO getCurrentTime
            LVar.set lvar t,
      siteRouteEncoder = routeEncoder
    }

main :: IO ()
main = do
  void $ Ema.runSite site

render :: RouteEncoder UTCTime Route -> UTCTime -> Route -> LByteString
render enc now r =
  tailwindLayout (H.title "Clock" >> H.base ! A.href "/") $
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
      A.href (fromString . toString $ Ema.routeUrl enc now r')
    randomColor t =
      let epochSecs = fromMaybe 0 . readMaybe @Int $ formatTime defaultTimeLocale "%s" t
          colors = ["green", "gray", "purple", "red", "blue", "yellow", "black", "pink"]
       in colors !! mod epochSecs (length colors)
