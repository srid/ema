module Ema.Route.Lib.Extra.MarkdownRoute where

import GHC.TypeLits (Symbol)
import Network.URI.Slug (Slug)

-- | Represents the relative path to a source .md file
newtype MarkdownRoute (baseDir :: Symbol) = MarkdownRoute {unMarkdownRoute :: NonEmpty Slug}
  deriving stock (Eq, Ord, Show, Generic)

data Model = Model
  { modelPandocs :: Map MarkdownRoute Pandoc
  }