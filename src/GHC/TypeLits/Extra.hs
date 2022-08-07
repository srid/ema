{-# LANGUAGE UndecidableInstances #-}

module GHC.TypeLits.Extra where

import GHC.TypeLits (TypeError)

class Impossible where
  impossible :: a

{- | Like TypeError but suitable for use in type-classes to avoid `undefined`

 cf. https://stackoverflow.com/a/72783230/55246
-}
type family TypeErr t where
  TypeErr t = (TypeError t, Impossible)
