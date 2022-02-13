-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Ema
import Ema.Example.Common (tailwindLayout)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Index
  | About
  deriving stock (Show, Enum, Bounded)

newtype Model = Model {unModel :: Text}

routeEncoder :: RouteEncoder a Route
routeEncoder =
  (enc, dec, all_)
  where
    enc _model =
      \case
        Index -> "index.html"
        About -> "about.html"
    dec _model = \case
      "index.html" -> Just Index
      "about.html" -> Just About
      _ -> Nothing
    all_ _ = defaultEnum @Route

site :: Site Model Route
site =
  Site
    { siteName = "Ex02",
      siteRender = \_ enc m r ->
        Ema.AssetGenerated Ema.Html $ render enc m r,
      siteModelPatcher = Ema.constModal $ Model "Hello World.",
      siteRouteEncoder = routeEncoder
    }

main :: IO ()
main = do
  void $ Ema.runSite site

render :: RouteEncoder Model Route -> Model -> Route -> LByteString
render enc model r =
  tailwindLayout (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        H.toHtml (unModel model)
        case r of
          Index -> do
            "You are on the index page. "
            routeElem About "Go to About"
          About -> do
            "You are on the about page. "
            routeElem Index "Go to Index"
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl enc model r')
