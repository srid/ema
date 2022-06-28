{-# LANGUAGE UndecidableInstances #-}

module GHC.TypeLits.Extra.Symbol (
  StripPrefix,
  ToLower,
  BeforeChar,
  AfterChar,
  SplitAtChar,
) where

import GHC.TypeLits (ConsSymbol, Symbol, UnconsSymbol)

-- | Strip `prefix` from `symbol`. Return `symbol` as-is if the prefix doesn't match.
type family StripPrefix (prefix :: Symbol) (symbol :: Symbol) :: Symbol where
  StripPrefix prefix symbol =
    FromMaybe
      symbol
      (StripPrefix' (UnconsSymbol prefix) (UnconsSymbol symbol))

-- | Strip `prefix` from `symbol`. Return Nothing if the prefix doesn't match.
type family StripPrefix' (prefix :: Maybe (Char, Symbol)) (symbol :: Maybe (Char, Symbol)) :: Maybe Symbol where
  StripPrefix' 'Nothing 'Nothing = 'Just ""
  StripPrefix' 'Nothing ( 'Just '(x, xs)) = 'Just (ConsSymbol x xs)
  StripPrefix' _p 'Nothing = 'Nothing
  StripPrefix' ( 'Just '(p, ps)) ( 'Just '(p, ss)) = StripPrefix' (UnconsSymbol ps) (UnconsSymbol ss)
  StripPrefix' ( 'Just '(p, ps)) ( 'Just '(_, ss)) = 'Nothing

type family ToLower (sym :: Symbol) :: Symbol where
  ToLower sym = ToLower' (UnconsSymbol sym)

type family ToLower' (pair :: Maybe (Char, Symbol)) :: Symbol where
  ToLower' 'Nothing = ""
  ToLower' ( 'Just '(c, cs)) = ConsSymbol (ToLowerC c) (ToLower' (UnconsSymbol cs))

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

type family BeforeChar (c :: Char) (sym :: Symbol) :: Symbol where
  BeforeChar c sym = BeforeChar' c (UnconsSymbol sym)

type family BeforeChar' (char :: Char) (pair :: Maybe (Char, Symbol)) :: Symbol where
  BeforeChar' char 'Nothing = ""
  BeforeChar' char ( 'Just '(char, cs)) = ""
  BeforeChar' char ( 'Just '(c,    cs)) = ConsSymbol c (BeforeChar' char (UnconsSymbol cs))

type family AfterChar (c :: Char) (sym :: Symbol) :: Symbol where
  AfterChar c sym = AfterChar' c (UnconsSymbol sym)

type family AfterChar' (char :: Char) (pair :: Maybe (Char, Symbol)) :: Symbol where
  AfterChar' char 'Nothing = ""
  AfterChar' char ( 'Just '(char, cs)) = cs
  AfterChar' char ( 'Just '(_,    cs)) = AfterChar' char (UnconsSymbol cs)

type family SplitAtChar (char :: Char) (sym :: Symbol) :: (Symbol, Symbol) where
  SplitAtChar char sym = '(BeforeChar char sym, AfterChar char sym)
