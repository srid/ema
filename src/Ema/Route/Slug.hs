{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Ema.Route.Slug where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import qualified Data.Text as T
import qualified Data.Text.Normalize as UT
import qualified Network.URI.Encode as UE

-- | An URL path is made of multiple slugs, separated by '/'
newtype Slug = Slug {unSlug :: Text}
  deriving (Eq, Show, Ord, Data, Generic, ToJSON, FromJSON)

-- | Decode an URL component into a `Slug` using `Network.URI.Encode`
decodeSlug :: Text -> Slug
decodeSlug =
  fromString . UE.decode . toString

-- | Encode a `Slug` into an URL component using `Network.URI.Encode`
encodeSlug :: Slug -> Text
encodeSlug =
  UE.encodeText . unSlug

instance IsString Slug where
  fromString :: HasCallStack => String -> Slug
  fromString (toText -> s) =
    if "/" `T.isInfixOf` s
      then error ("Slug cannot contain a slash: " <> s)
      else Slug (unicodeNormalize s)

-- Normalize varying non-ascii strings (in filepaths / slugs) to one
-- representation, so that they can be reliably linked to.
unicodeNormalize :: Text -> Text
unicodeNormalize = UT.normalize UT.NFC . toText
