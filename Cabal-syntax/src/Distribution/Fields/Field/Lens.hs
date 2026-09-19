{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Fields.Field.Lens where

import Distribution.Compat.Lens
import qualified Distribution.Fields.Field as T
import Distribution.Parsec.Position.Lens

priorComments :: Lens' (T.WithComments ann) [T.Comment ann]
priorComments f s = fmap (\x -> s{T.priorComments = x}) (f (T.priorComments s))
{-# INLINE priorComments #-}

followingComments :: Lens' (T.WithComments ann) [T.Comment ann]
followingComments f s = fmap (\x -> s{T.followingComments = x}) (f (T.followingComments s))
{-# INLINE followingComments #-}

unComments :: Lens' (T.WithComments ann) ann
unComments f s = fmap (\x -> s{T.unComments = x}) (f (T.unComments s))
{-# INLINE unComments #-}

instance HasPosition ann => HasPosition (T.WithComments ann) where
  position = unComments . position
