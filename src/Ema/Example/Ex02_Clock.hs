{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

{-# HLINT ignore "Use infinitely" #-}

{- | A very simple site with routes, but based on dynamically changing values

 The current time is computed in the server every second, and the resultant
 generated HTML is automatically updated on the browser. This is only a demo;
 usually we render HTML based on files on disk or something accessible outside
 of the browser. More advanced examples will demonstrate that.
-}
module Ema.Example.Ex02_Clock where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (logDebugNS, logInfoNS)
import Data.List ((!!))
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Route.Encoder (RouteEncoder)
import Generics.SOP qualified as SOP
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

type Model = UTCTime

data Route
  = Route_Index
  | Route_OnlyTime
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model Route)

instance EmaSite Route where
  siteInput _ _ () = do
    t0 <- liftIO getCurrentTime
    pure . Dynamic . (t0,) $ \setModel -> do
      logInfoNS "Ex02" "Starting clock..."
      forever $ do
        liftIO $ threadDelay 1000000
        t <- liftIO getCurrentTime
        logDebugNS "Ex02" "Updating clock..."
        setModel t
  siteOutput enc m r =
    Ema.AssetGenerated Ema.Html $ render enc m r

main :: IO ()
main = do
  void $ Ema.runSite @Route ()

render :: RouteEncoder UTCTime Route -> UTCTime -> Route -> LByteString
render enc now r =
  tailwindLayout (H.title "Clock" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        case r of
          Route_Index ->
            "The current date & time is: "
          Route_OnlyTime ->
            "The current time is: "
        H.pre ! A.class_ "text-6xl font-bold mt-2" $ do
          H.span ! A.class_ ("text-" <> randomColor now <> "-500") $ do
            let fmt = case r of
                  Route_Index -> "%Y/%m/%d %H:%M:%S"
                  Route_OnlyTime -> "%H:%M:%S"
            H.toMarkup $ formatTime defaultTimeLocale fmt now
      H.div ! A.class_ "mt-4 text-center" $ do
        case r of
          Route_Index -> do
            routeElem Route_OnlyTime "Hide day?"
          Route_OnlyTime -> do
            routeElem Route_Index "Show day?"
  where
    routeElem r' = H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r'
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl enc now r')
    randomColor t =
      let epochSecs = fromMaybe 0 . readMaybe @Int $ formatTime defaultTimeLocale "%s" t
          colors = ["green", "gray", "purple", "red", "blue", "yellow", "black", "pink"]
       in colors !! mod epochSecs (length colors)
