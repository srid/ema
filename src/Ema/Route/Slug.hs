{-# LANGUAGE InstanceSigs #-}

module Ema.Route.Slug where

import qualified Data.Text as T

-- ---- [Slug] ----

newtype Slug = Slug {unSlug :: Text}
  deriving (Eq)

instance IsString Slug where
  fromString :: HasCallStack => String -> Slug
  fromString (toText -> s) =
    if "/" `T.isInfixOf` s
      then error ("Slug cannot contain a slash: " <> s)
      else Slug s
