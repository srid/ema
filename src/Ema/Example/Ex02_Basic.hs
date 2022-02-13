-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Ema qualified
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex03_Clock qualified as Ex03
import Ema.Site
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Index
  | About
  deriving stock (Show, Enum, Bounded)

newtype Model = Model {unModel :: Text}

routeEncoder :: PartialIsoEnumerableWithCtx m FilePath Route
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

main :: IO ()
main = do
  let site :: Site Route Model =
        Site
          { siteRender = \_ enc m r ->
              Ema.AssetGenerated Ema.Html $ render enc m r,
            siteModelPatcher = \_ set -> do
              set (Model "Hello World.") $ \_lvar ->
                pure (),
            siteRouteEncoder = routeEncoder
          }
  void $
    Ema.runEma $
      siteUnder @"hello" site
        `eitherSites` Ex03.site

render :: PartialIsoEnumerableWithCtx Model FilePath Route -> Model -> Route -> LByteString
render enc model r =
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
      A.href (fromString . toString $ Ema.routeUrl enc model r')
