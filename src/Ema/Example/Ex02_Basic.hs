-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (logInfoNS)
import Data.LVar qualified as LVar
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Route (unsafeMkRouteEncoder)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Index
  | About
  deriving stock (Show, Eq, Enum, Bounded)

newtype Model = Model {modelMsg :: Text}

routeEncoder :: RouteEncoder a Route
routeEncoder =
  unsafeMkRouteEncoder enc dec all_
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
      -- TODO: Can this be a reader too?
      siteRender = SiteRender $ \m r -> do
        enc <- askRouteEncoder
        pure $ Ema.AssetGenerated Ema.Html $ render enc m r,
      siteModelManager = ModelManager $ do
        pure $
          X
            ( Model "Hello!",
              \set -> do
                logInfoNS "Ex02" "Setting 2nd time"
                -- LVar.modify lvar $ \_ -> Model "Hello, again."
                set $ Model "Hello, again"
                -- Normally you would update the model over time.
                liftIO $ threadDelay maxBound
            ),
      siteRouteEncoder = routeEncoder
    }

main :: IO ()
main = do
  void $ Ema.runSite site

render :: RouteEncoder Model Route -> Model -> Route -> LByteString
render enc model@(Model msg) r =
  tailwindLayout (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        H.p $ H.em $ H.toHtml msg
        case r of
          Index -> do
            "You are on the index page. "
            routeElem About "Go to About"
          About -> do
            routeElem Index "Go to Index"
            ". You are on the about page. "
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl enc model r')
