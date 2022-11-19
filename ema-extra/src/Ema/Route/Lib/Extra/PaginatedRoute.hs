{-# LANGUAGE DeriveAnyClass #-}

module Ema.Route.Lib.Extra.PaginatedRoute (
  -- * Page
  Page,

  -- * Functions
  pageNum,
  lookupPage,
  lookupPage',
) where

import Data.Default (Default (..))
import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (toPrism_)
import Generics.SOP qualified as SOP
import Optics.Core (prism')

newtype Page (t :: Type) = Page {unPage :: Word}
  deriving newtype (Show, Eq, Ord, Num, Enum, Default)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

pageNum :: Page a -> Text
pageNum = \case
  0 -> "1"
  n -> show $ n + 1

parsePageNum :: String -> Maybe (Page a)
parsePageNum s = do
  n <- readMaybe s
  pure $ Page $ n - 1

lookupPage :: HasCallStack => Page a -> NonEmpty [a] -> [a]
lookupPage r xs =
  fromMaybe (error outOfBoundsError) $ lookupPage' r xs
  where
    outOfBoundsError =
      "lookupPage: Page "
        <> show r
        <> " is out of bounds of total available pages: "
        <> show (length xs)

lookupPage' :: Page a -> NonEmpty [a] -> Maybe [a]
lookupPage' p xs =
  toList xs !!? (fromInteger . toInteger $ unPage p)

instance IsRoute (Page a) where
  type RouteModel (Page a) = NonEmpty [a]
  routePrism m =
    -- TODO: Refactor this using lens composition.
    toPrism_ $
      prism'
        ( \case
            Page 0 -> "index.html"
            p -> "page/" <> toString (pageNum p) <> ".html"
        )
        ( \fp -> do
            if fp == "index.html"
              then pure def
              else do
                page <- fmap toString $ T.stripSuffix ".html" =<< T.stripPrefix "page/" (toText fp)
                r <- parsePageNum page
                void $ lookupPage' r m -- Check if this page exists
                pure r
        )
  routeUniverse m =
    [1 :: (Page a) .. (fromInteger . toInteger $ length m)]
