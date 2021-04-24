{-# LANGUAGE InstanceSigs #-}

module Ema.Route.Slug where

import qualified Data.Text as T

-- | An URL path is made of multiple slugs, separated by '/'
newtype Slug = Slug {unSlug :: Text}
  deriving (Eq, Show)

instance IsString Slug where
  fromString :: HasCallStack => String -> Slug
  fromString (toText -> s) =
    if "/" `T.isInfixOf` s
      then error ("Slug cannot contain a slash: " <> s)
      else Slug s
