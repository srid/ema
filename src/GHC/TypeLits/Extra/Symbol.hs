{-# LANGUAGE UndecidableInstances #-}

-- TODO: Upstream this to symbols package
-- https://github.com/kcsongor/symbols/issues/3
module GHC.TypeLits.Extra.Symbol (
  StripPrefix,
  ToLower,
) where

import GHC.TypeLits

type family StripPrefix (prefix :: Symbol) (symbol :: Symbol) :: Symbol where
  StripPrefix prefix symbol =
    FromMaybe
      symbol
      (StripPrefixCons (UnconsSymbol prefix) (UnconsSymbol symbol))

type family Cons (pair :: Maybe (Char, Symbol)) :: Symbol where
  Cons 'Nothing = ""
  Cons ( 'Just '(a, b)) = ConsSymbol a b

type family StripPrefixCons (prefix :: Maybe (Char, Symbol)) (symbol :: Maybe (Char, Symbol)) :: Maybe Symbol where
  StripPrefixCons 'Nothing s = 'Just (Cons s)
  StripPrefixCons _ 'Nothing = 'Nothing
  StripPrefixCons ( 'Just '(p, ps)) ( 'Just '(p, ss)) = StripPrefixCons (UnconsSymbol ps) (UnconsSymbol ss)
  StripPrefixCons ( 'Just '(p, ps)) ( 'Just '(_, ss)) = 'Nothing

type family ToLower (sym :: Symbol) :: Symbol where
  ToLower sym = ToLowerCons (UnconsSymbol sym)

type family ToLowerCons (pair :: Maybe (Char, Symbol)) :: Symbol where
  ToLowerCons 'Nothing = ""
  ToLowerCons ( 'Just '(c, cs)) = ConsSymbol (ToLowerC c) (ToLowerCons (UnconsSymbol cs))

type family ToLowerC (c :: Char) :: Char where
  ToLowerC 'A' = 'a'
  ToLowerC 'B' = 'b'
  ToLowerC 'C' = 'c'
  ToLowerC 'D' = 'd'
  ToLowerC 'E' = 'e'
  ToLowerC 'F' = 'f'
  ToLowerC 'G' = 'g'
  ToLowerC 'H' = 'h'
  ToLowerC 'I' = 'i'
  ToLowerC 'J' = 'j'
  ToLowerC 'K' = 'k'
  ToLowerC 'L' = 'l'
  ToLowerC 'M' = 'm'
  ToLowerC 'N' = 'n'
  ToLowerC 'O' = 'o'
  ToLowerC 'P' = 'p'
  ToLowerC 'Q' = 'q'
  ToLowerC 'R' = 'r'
  ToLowerC 'S' = 's'
  ToLowerC 'T' = 't'
  ToLowerC 'U' = 'u'
  ToLowerC 'V' = 'v'
  ToLowerC 'W' = 'w'
  ToLowerC 'X' = 'x'
  ToLowerC 'Y' = 'y'
  ToLowerC 'Z' = 'z'
  ToLowerC a = a

type family FromMaybe (def :: a) (maybe :: Maybe a) :: a where
  FromMaybe def 'Nothing = def
  FromMaybe def ( 'Just a) = a
