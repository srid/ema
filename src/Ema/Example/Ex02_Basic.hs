{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar
import Ema (Ema (..))
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

instance Ema Model Route where
  encodeRoute =
    \case
      Index -> "index.html"
      About -> "about.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    "about.html" -> Just About
    _ -> Nothing

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \model -> do
    LVar.set model $ Model "Hello World. "
    liftIO $ threadDelay maxBound

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction (Model s) r =
  Tailwind.layout emaAction (H.title "Basic site" >> H.base ! A.href "/") $
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
