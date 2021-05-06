{-# LANGUAGE InstanceSigs #-}

module Ema.Route.Slug where

import qualified Data.Text as T
import qualified Data.Text.Normalize as UT

-- | An URL path is made of multiple slugs, separated by '/'
newtype Slug = Slug {unSlug :: Text}
  deriving (Eq, Show, Ord)

instance IsString Slug where
  fromString :: HasCallStack => String -> Slug
  fromString (toText -> s) =
    if "/" `T.isInfixOf` s
      then error ("Slug cannot contain a slash: " <> s)
      else Slug (unicodeNormalize s)
    where
      -- Normalize varying non-ascii strings (in filepaths / slugs) to one
      -- representation, so that they can be reliably linked to.
      unicodeNormalize = UT.normalize UT.NFC . toText
