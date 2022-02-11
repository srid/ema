{-# LANGUAGE DeriveAnyClass #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Control.Concurrent (threadDelay)
import Data.LVar qualified as LVar
import Data.Universe (Finite, Universe (..), universeGeneric)
import Ema (Ema (..))
import Ema qualified
import Ema.CLI qualified as CLI
import Ema.Example.Common (tailwindLayout)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Index
  | About
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Finite)

instance Universe Route where universe = universeGeneric

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
  void $
    Ema.runEma (\_act m -> Ema.AssetGenerated Ema.Html . render m) $ \_act model -> do
      LVar.set model $ Model "Hello World. "
      liftIO $ threadDelay maxBound

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
