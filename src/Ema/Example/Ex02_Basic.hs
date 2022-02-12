-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Control.Concurrent (threadDelay)
import Ema (Ema (..))
import Ema qualified
import Ema.Example.Common (tailwindLayout)
import Ema.Site
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Index
  | About
  deriving stock (Show, Enum, Bounded)

newtype Model = Model {unModel :: Text}

instance Ema Route where
  type ModelFor Route = Model
  encodeRoute _model =
    \case
      Index -> "index.html"
      About -> "about.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    "about.html" -> Just About
    _ -> Nothing

main :: IO ()
main = do
  let site :: Site Route =
        Site
          { siteRender = \_ m r ->
              Ema.AssetGenerated Ema.Html $ render m r,
            siteModelPatcher = \_ set -> do
              void $ set $ Model "Hello World."
          }
  void $ Ema.runEma site

render :: Model -> Route -> LByteString
render model r =
  tailwindLayout (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        case r of
          Index -> do
            H.toHtml (unModel model)
            "You are on the index page. "
            routeElem About "Go to About"
          About -> do
            "You are on the about page. "
            routeElem Index "Go to Index"
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
