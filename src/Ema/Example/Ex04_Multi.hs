-- | Demonstration of merging multiple sites
module Ema.Example.Ex04_Multi where

import Ema ((+:))
import Ema qualified
import Ema.Example.Common (tailwindLayout)
import Ema.Example.Ex02_Basic qualified as Ex02
import Ema.Example.Ex03_Clock qualified as Ex03
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

main :: IO ()
main = do
  let site = Ema.singlePageSite $ const renderIndex
  void $
    Ema.runEma $
      site
        +: Ema.mountUnder "basic" Ex02.site
        +: Ema.mountUnder "clock" Ex03.site

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
