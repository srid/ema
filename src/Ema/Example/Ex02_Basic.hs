{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Ema.Example.Ex02_Basic where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (logInfoNS)
import Ema
import Ema.Example.Common (tailwindLayout)
import Ema.Route.Encoder (RouteEncoder)
import Ema.Route.Generic
import GHC.Generics qualified as GHC
import Generics.SOP
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (Generic)

data Route
  = Route_Index
  | Route_About
  deriving stock (Show, Eq, Enum, Bounded)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo)
  deriving (IsRoute) via (ConstModelRoute Model Route)

newtype Model = Model {modelMsg :: Text}

main :: IO ()
main = do
  void $ Ema.runSite @Route ()

instance HasModel Route where
  type ModelInput Route = ()
  runModel _ _ () = do
    pure $
      Dynamic
        ( Model "Hello!",
          \set -> do
            logInfoNS "Ex02" "Setting 2nd time"
            -- LVar.modify lvar $ \_ -> Model "Hello, again."
            set $ Model "Hello, again"
            -- Normally you would update the model over time.
            liftIO $ threadDelay maxBound
        )

instance RenderAsset Route where
  renderAsset enc m r =
    Ema.AssetGenerated Ema.Html $ render enc m r

render :: RouteEncoder Model Route -> Model -> Route -> LByteString
render enc model@(Model msg) r =
  tailwindLayout (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        H.p $ H.em $ H.toHtml msg
        case r of
          Route_Index -> do
            "You are on the index page. "
            routeElem Route_About "Go to About"
          Route_About -> do
            routeElem Route_Index "Go to Index"
            ". You are on the about page. "
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl enc model r')
