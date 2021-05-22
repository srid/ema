{-# LANGUAGE TypeApplications #-}

-- | A very simple site with routes, but based on dynamically changing values
--
-- The current time is computed in the server every second, and the resultant
-- generated HTML is automatically updated on the browser. This is only a demo;
-- usually we render HTML based on files on disk or something accessible outside
-- of the browser. More advanced examples will demonstrate that.
module Ema.Example.Ex02_Basic where

import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar
import Ema (FileRoute (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  | About
  deriving (Show, Enum, Bounded)

data Model = Model Text

instance FileRoute Route where
  encodeFileRoute =
    \case
      Index -> "index.html"
      About -> "about.html"
  decodeFileRoute = \case
    "index.html" -> Just Index
    "about.html" -> Just About
    _ -> Nothing

main :: IO ()
main = do
  let routes = [minBound .. maxBound]
  Ema.runEma (const $ Right <$> routes) render $ \model -> do
    LVar.set model $ Model "Hello World. "
    liftIO $ threadDelay maxBound

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction (Model s) r =
  Tailwind.layout emaAction (H.title "Basic site") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        case r of
          Index -> do
            H.toHtml s
            "You are on the index page. "
            routeElem About "Go to About"
          About -> do
            "You are on the about page. "
            routeElem Index "Go to Index"
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl r')
