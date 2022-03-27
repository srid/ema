module Ema.Example.Common (
  tailwindLayout,
) where

import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

-- | A simple and off-the-shelf layout using Tailwind CSS
tailwindLayout :: H.Html -> H.Html -> LByteString
tailwindLayout h b =
  layoutWith "en" "UTF-8" (tailwind2ShimCdn >> h) $
    -- The "overflow-y-scroll" makes the scrollbar visible always, so as to
    -- avoid janky shifts when switching to routes with suddenly scrollable content.
    H.body ! A.class_ "overflow-y-scroll" $ b
  where
    -- A general layout
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

    -- Loads full tailwind CSS from CDN (not good for production)
    tailwind2ShimCdn :: H.Html
    tailwind2ShimCdn =
      H.link
        ! A.href "https://unpkg.com/tailwindcss@2/dist/tailwind.min.css"
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
