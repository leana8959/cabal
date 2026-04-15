module Distribution.Fields.Field.Lens where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Position.Lens
import Distribution.Fields.Field (WithComments)
import qualified Distribution.Fields.Field as T

-- TODO(leana8959):
-- Not sure if this is unlawful because it focus on one point instead
-- of all 'Position's in the structure
instance HasPosition a => HasPosition (WithComments a) where
  position f s = fmap (\x -> s{T.unComments = x}) (position f (T.unComments s))
