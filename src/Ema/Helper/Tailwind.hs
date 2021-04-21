{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Use Tailwind? Try this module for rapid prototyping of websites in Ema.
module Ema.Helper.Tailwind
  ( -- * Main functions
    layout,
    layoutWith,

    -- * Tailwind shims
    twindShimCdn,
    twindShimOfficial,
    twindShimUnofficial,
  )
where

import NeatInterpolation (text)
import qualified Text.Blaze.Html.Renderer.Utf8 as RU
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | A simple and off-the-shelf layout using Tailwind CSS
layout :: H.Html -> H.Html -> LByteString
layout =
  layoutWith twindShimCdn

-- | Like @layout@, but pick your own tailwind shim.
layoutWith :: H.Html -> H.Html -> H.Html -> LByteString
layoutWith twindShim appHead appBody = RU.renderHtml $ do
  H.docType
  H.html ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      appHead
      twindShim
    H.body $ do
      appBody

-- | Loads full tailwind CSS from CDN (not good for production)
twindShimCdn :: H.Html
twindShimCdn =
  H.unsafeByteString . encodeUtf8 $
    [text|
  <link href="https://unpkg.com/tailwindcss@2.1.1/dist/tailwind.min.css" rel="stylesheet" type="text/css">
  |]

-- | This shim doesn't work with hot reload.
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
  H.script
    ! A.type_ "text/javascript"
    $ (fromString . toString) twindShimJS
  where
    twindShimJS :: Text
    twindShimJS =
      [text|
      twind.setup({})
      twindObserve.observe(document.documentElement)
    |]
