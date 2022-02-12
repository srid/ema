-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Control.Concurrent (threadDelay)
import Data.LVar qualified as LVar
import Data.Some
import Ema (Ema (..))
import Ema qualified
import Ema.CLI qualified as CLI
import Ema.Example.Common (tailwindLayout)
import Ema.Site
import Relude (LByteString)
import Relude.Extra.Lens
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
  encodeRoute _model (traceShowId -> r) =
    case r of
      Index -> "index.html"
      About -> "about.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    "about.html" -> Just About
    _ -> Nothing

main :: IO ()
main = do
  site1 :: Site Route <- Ema.mkSite renderRoute $ \_act model -> do
    liftIO $ print "foo"
    LVar.set model $ Model "Hello World."
    liftIO $ threadDelay maxBound
  -- /hello
  let site :: Site (RoutePrefix "hello" Route) =
        siteUnder @"hello" site1
  void $ Ema.runEma $ traceShow "site" site

renderRoute ::
  forall r.
  (Ema r, ModelFor r ~ Model) =>
  Some CLI.Action ->
  Lens' r Route ->
  Model ->
  r ->
  Ema.Asset LByteString
renderRoute _act iso model r =
  Ema.AssetGenerated Ema.Html $ render iso model $ traceShow "ff" r

render ::
  forall r'.
  (Ema r', ModelFor r' ~ Model) =>
  Lens' r' Route ->
  Model ->
  r' ->
  LByteString
render iso model r =
  tailwindLayout (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        let url = Ema.routeUrl model About
        -- curr: /about
        -- exp: /hello/about
        case view iso r of
          Index -> do
            H.toHtml (unModel model)
            "You are on the index page. "
            routeElem (set iso About $ traceShow "ro" r) "Go to About"
          About -> do
            "You are on the about page. "
            routeElem (set iso Index r) "Go to Index"
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl @r' model r')
