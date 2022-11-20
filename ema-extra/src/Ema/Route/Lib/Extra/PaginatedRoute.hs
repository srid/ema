{-# LANGUAGE DeriveAnyClass #-}

-- | Provides a `Page` type that enables any route to be paginated.
module Ema.Route.Lib.Extra.PaginatedRoute (
  -- * Page
  Page,

  -- * `Page` Functions
  pageNum,
  fromNum,
  pageRange,
  lookupPage,
  lookupPage',

  -- * Utility functions
  paginate,
) where

import Data.Default (Default (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Ema.Route.Class (IsRoute (..))
import Ema.Route.Prism (toPrism_)
import Generics.SOP qualified as SOP
import Optics.Core (prism')
import Text.Show qualified as Show

{- | Represents a single page in a multi-page view/route.

  Use `pageNum` to retrieve the user-facing page number which are 1-index;
  `fromNum` to convert them back.
-}
newtype Page (t :: Type) = Page {unPage :: Word}
  deriving newtype (Eq, Ord, Num, Enum, Default)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Show (Page t) where
  show p = "Page " <> show (pageNum p)

-- | Get the user-facing page number.
pageNum :: forall a. Page a -> Int
pageNum (Page n) =
  fromInteger . toInteger $ n + 1

-- | Convert the user-facing page number.
fromNum :: forall a. Int -> Maybe (Page a)
fromNum n = do
  guard $ n > 0
  pure $ fromInteger . toInteger $ n - 1

-- | Enumerate list of all pages given the total number of pages.
pageRange :: forall a. HasCallStack => Int -> NonEmpty (Page a)
pageRange total =
  fromMaybe (error "pageRange: total must be positive and non-zero") $ do
    end <- fromNum @a total
    nonEmpty [def .. end]

-- | Retrieve the given page from the list.
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
        ( \page ->
            if page == def
              then "index.html"
              else "page/" <> show (pageNum page) <> ".html"
        )
        ( \fp -> do
            if fp == "index.html"
              then pure def
              else do
                page <- fmap toString $ T.stripSuffix ".html" =<< T.stripPrefix "page/" (toText fp)
                r <- fromNum <=< readMaybe $ page
                void $ lookupPage' r m -- Check if this page exists
                pure r
        )
  routeUniverse =
    toList . pageRange . length

-- | Break a list into pages given the page size.
paginate :: Int -> [a] -> NonEmpty [a]
paginate pageSize =
  fromMaybe (one mempty)
    . nonEmpty
    . fmap toList
    . toList
    . Seq.chunksOf pageSize
    . Seq.fromList