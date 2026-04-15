module Distribution.Parsec.Position.Lens where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Position (Position)

-- | Classy lens to discover positions in annotations
class HasPosition a where
  position :: Lens' a Position

instance HasPosition Position where
  position = id
