{-# LANGUAGE QuasiQuotes #-}

-- | Use Tailwind CSS with blaze-html? Try this module for rapid prototyping of
-- websites in Ema.
module Ema.Helper.Blaze
  ( -- * Main functions
    layoutWith,
    twindLayout,

    -- * Tailwind official shims
    tailwind2ShimCdn,

    -- * Twind.dev shims
    twindShimOfficial,
    twindShimUnofficial,
  )
where

import Data.Some (Some (Some))
import Ema.CLI qualified
import NeatInterpolation (text)
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

-- | A general layout
layoutWith :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html -> LByteString
layoutWith lang encoding appHead appBody = RU.renderHtml $ do
  H.docType
  H.html ! A.lang lang $ do
    H.head $ do
      H.meta ! A.charset encoding
      -- This makes the site mobile friendly by default.
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      appHead
    appBody

-- | A simple and off-the-shelf layout using Tailwind CSS
twindLayout :: Some Ema.CLI.Action -> H.Html -> H.Html -> LByteString
twindLayout action h b =
  layoutWith "en" "UTF-8" (shim >> h) $
    -- The "overflow-y-scroll" makes the scrollbar visible always, so as to
    -- avoid janky shifts when switching to routes with suddenly scrollable content.
    H.body ! A.class_ "overflow-y-scroll" $ b
  where
    shim :: H.Html
    shim =
      case action of
        Some (Ema.CLI.Generate _) ->
          twindShimUnofficial
        _ ->
          -- Twind shim doesn't reliably work in dev server mode. Let's just use the
          -- tailwind CDN.
          tailwind2ShimCdn

-- | Loads full tailwind CSS from CDN (not good for production)
tailwind2ShimCdn :: H.Html
tailwind2ShimCdn =
  H.link
    ! A.href "https://unpkg.com/tailwindcss@2/dist/tailwind.min.css"
    ! A.rel "stylesheet"
    ! A.type_ "text/css"

-- | This shim may not work with hot reload.
twindShimOfficial :: H.Html
twindShimOfficial =
  H.unsafeByteString . encodeUtf8 $
    [text|
    <script type="module" src="https://cdn.skypack.dev/twind/shim"></script>
    |]

-- | This shim does work with hot reload, but it spams console with warnings.
twindShimUnofficial :: H.Html
twindShimUnofficial = do
  H.script
    ! A.type_ "text/javascript"
    ! A.src "https://cdn.jsdelivr.net/combine/npm/twind/twind.umd.min.js,npm/twind/observe/observe.umd.min.js"
    $ ""
  H.script ! A.type_ "text/javascript" $ twindShimUnofficialEval
  where
    twindShimUnofficialEval :: H.Html
    twindShimUnofficialEval =
      H.unsafeByteString . encodeUtf8 $
        [text|
        // Be silent to avoid complaining about non-tailwind classes
        // https://github.com/tw-in-js/twind/discussions/180#discussioncomment-678272
        console.log("ema: Twind: setup & observe")
        twind.setup({mode: 'silent'})
        window.emaTwindObs = twindObserve.observe(document.documentElement);
        |]
