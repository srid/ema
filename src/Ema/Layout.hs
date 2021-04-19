{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ema.Layout where

import NeatInterpolation (text)
import qualified Text.Blaze.Html.Renderer.Utf8 as RU
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | A simple and off-the-shelf layout using Tailwind CSS
-- TODO: Allow customizing twindShim preflight classes (see below), including google fonts.
tailwindSite :: H.Html -> H.Html -> LByteString
tailwindSite appHead appBody = RU.renderHtml $ do
  H.docType
  H.html ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      appHead
      googleFonts
      -- Using shim doesn't work very well with hot-reload.
      -- TODO: Conditionally use shim, when statically generating the site
      -- twindShim'
      twindCdn
    H.body $ do
      appBody

twindCdn :: H.Html
twindCdn =
  H.unsafeByteString . encodeUtf8 $
    [text|
  <link href="https://unpkg.com/tailwindcss@2.1.1/dist/tailwind.min.css" rel="stylesheet" type="text/css">
  |]

twindShim :: H.Html
twindShim =
  H.unsafeByteString . encodeUtf8 $
    [text|
    <script type="module" src="https://cdn.skypack.dev/twind/shim"></script>
    |]

twindShim' :: H.Html
twindShim' = do
  H.script
    ! A.type_ "text/javascript"
    ! A.src "https://cdn.jsdelivr.net/combine/npm/twind/twind.umd.min.js,npm/twind/observe/observe.umd.min.js"
    $ ""
  H.script
    ! A.type_ "text/javascript"
    $ (fromString . toString) twindShimJS'

twindShimJS' :: Text
twindShimJS' =
  [text|
    twind.setup({ 
        theme: {
          extend: {
            fontFamily: (theme) => ({
              sans: ["DM Sans", theme("fontFamily.sans")],
              serif: ["Roboto", theme("fontFamily.serif")]
            })
          }
        },
        preflight: {
            body: {
                "@apply": "text-gray-700"
            },
            "a:any-link": {
                "@apply": "no-underline hover:font-bold"
            },
            p: {
                "@apply": "mb-2"
            },
            "header.pageHeader": {
                "@apply": "mb-4 border-l-4 pl-2 hover:(shadow-sm bg-pink-50)"
            }
        },
    })
    twindObserve.observe(document.documentElement)
  |]

googleFonts :: H.Markup
googleFonts =
  -- Using unsafe string only for convenience when copy-pasting from Google Fonts.
  H.unsafeByteString . encodeUtf8 $
    [text|
          <link rel="preconnect" href="https://fonts.gstatic.com">
          <link href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@500&family=Roboto&display=swap" rel="stylesheet">
        |]
