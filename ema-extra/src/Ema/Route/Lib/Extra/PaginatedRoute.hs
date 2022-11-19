{-# LANGUAGE DeriveAnyClass #-}

module Ema.Route.Lib.Extra.PaginatedRoute (
  -- * Page
  Page,

  -- * PaginatedRoute
  PaginatedRoute (..),

  -- * Functions
  getPage,
  fromPage,
  lookupPage,
  lookupPage',
) where

import Data.Default (Default (..))
import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (toPrism_)
import Generics.SOP qualified as SOP
import Optics.Core (prism')

newtype Page = Page {unPage :: Word}
  deriving newtype (Show, Eq, Ord, Num, Enum)

data PaginatedRoute (t :: Type) = PaginatedRoute_Main | PaginatedRoute_OnPage Page
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Default (PaginatedRoute t) where
  def = PaginatedRoute_Main

getPage :: forall {t}. PaginatedRoute t -> Page
getPage = \case
  PaginatedRoute_Main -> Page 1
  PaginatedRoute_OnPage p -> p

fromPage :: forall {t}. Page -> PaginatedRoute t
fromPage = \case
  Page 1 -> PaginatedRoute_Main
  p -> PaginatedRoute_OnPage p

lookupPage :: HasCallStack => PaginatedRoute a -> NonEmpty [a] -> [a]
lookupPage r xs =
  fromMaybe (error outOfBoundsError) $ lookupPage' r xs
  where
    outOfBoundsError =
      "lookupPage: Page "
        <> show (getPage r)
        <> " is out of bounds of total available pages: "
        <> show (length xs)

lookupPage' :: PaginatedRoute a -> NonEmpty [a] -> Maybe [a]
lookupPage' r xs =
  toList xs !!? (fromInteger . toInteger $ unPage (getPage r) - 1)

instance IsRoute (PaginatedRoute a) where
  type RouteModel (PaginatedRoute a) = NonEmpty [a]
  routePrism m =
    -- TODO: Refactor this using lens composition.
    toPrism_ $
      prism'
        ( \case
            PaginatedRoute_Main -> "index.html"
            PaginatedRoute_OnPage page -> "page/" <> show (unPage page) <> ".html"
        )
        ( \fp -> do
            if fp == "index.html"
              then pure PaginatedRoute_Main
              else do
                page <- fmap toString $ T.stripSuffix ".html" =<< T.stripPrefix "page/" (toText fp)
                p <- Page <$> readMaybe page
                let r = fromPage p
                void $ lookupPage' r m -- Check if this page exists
                pure r
        )
  routeUniverse m =
    fromPage <$> [1 :: Page .. (fromInteger . toInteger $ length m)]
