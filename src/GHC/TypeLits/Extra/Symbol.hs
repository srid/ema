{-# LANGUAGE UndecidableInstances #-}

-- TODO: Upstream this to symbols package
module GHC.TypeLits.Extra.Symbol where

import Data.Symbol.Ascii
import GHC.TypeLits

type family StripPrefix (prefix :: Symbol) (symbol :: Symbol) :: Symbol where
  StripPrefix prefix symbol =
    FromList
      ( FromMaybe
          (ToList symbol)
          ( StripPrefixL (ToList prefix) (ToList symbol)
          )
      )

type family StripPrefixL (prefix :: [Symbol]) (symbol :: [Symbol]) :: Maybe [Symbol] where
  StripPrefixL '[] symbol = 'Just symbol
  StripPrefixL _ '[] = 'Just '[]
  StripPrefixL (p ': ps) (p ': ss) = StripPrefixL ps ss
  StripPrefixL (p ': ps) (s ': ss) = 'Nothing

type family FromMaybe (def :: a) (maybe :: Maybe a) :: a where
  FromMaybe def 'Nothing = def
  FromMaybe def ( 'Just a) = a

type family FromList (list :: [Symbol]) :: Symbol where
  FromList '[] = ""
  FromList (x ': xs) = AppendSymbol x (FromList xs)
