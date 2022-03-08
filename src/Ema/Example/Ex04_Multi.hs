-- | Demonstration of merging multiple sites
module Ema.Example.Ex04_Multi where

import Ema qualified
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex02_Basic qualified as Ex02
import Ema.Example.Ex03_Clock qualified as Ex03
import Ema.Route.Encoder (Mergeable (merge), RouteEncoder, allRoutes, decodeRoute, encodeRoute, unsafeMkRouteEncoder)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data R
  = RIndex
  | RBasicSite Ex02.Route
  | RClockSite Ex03.Route

rEncoder :: RouteEncoder (Ex02.Model, Ex03.Model) R
rEncoder =
  unsafeMkRouteEncoder enc dec all_
  where
    enc (m2, m3) = \case
      RIndex -> "index.html"
      RBasicSite r ->
        encodeRoute Ex02.routeEncoder m2 r
      RClockSite r ->
        encodeRoute Ex03.routeEncoder m3 r
    dec (m2, m3) = \case
      "index.html" -> Just RIndex
      fp ->
        fmap RBasicSite (decodeRoute Ex02.routeEncoder m2 fp)
          <|> fmap RClockSite (decodeRoute Ex03.routeEncoder m3 fp)
    all_ (m2, m3) =
      [RIndex]
        <> fmap RBasicSite (allRoutes Ex02.routeEncoder m2)
        <> fmap RClockSite (allRoutes Ex03.routeEncoder m3)

main :: IO ()
main = do
  Ema.runSite_ $
    Ema.singlePageSite "index" renderIndex
      -- TODO: Can we 'decompose' routeencoder, so as to be able to use ADT to compose sites?
      `merge` Ema.mountUnder @"basic" Ex02.site
      `merge` Ema.mountUnder @"clock" Ex03.site

renderIndex :: LByteString
renderIndex =
  tailwindLayout (H.title "MultiSite" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto text-center mt-8 p-2" $ do
      H.p "You can compose Ema sites. Here are two sites composed to produce one:"
      H.ul ! A.class_ "flex flex-col justify-center .items-center mt-4 space-y-4" $ do
        H.li $ routeElem "basic" "Ex02_Basic"
        H.li $ routeElem "clock" "Ex03_Clock"
  where
    routeElem url w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! A.href url $ w
