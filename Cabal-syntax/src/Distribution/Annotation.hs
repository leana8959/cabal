{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

module Distribution.Annotation where

import qualified Data.ByteString as BS
import Distribution.Parsec.Position
import Distribution.Fields.Field

import Distribution.Parsec

data LocalSrcSpan = LocalSrcSpan {-# UNPACK #-} !RelPosition {-# UNPACK #-} !RelPosition
  deriving (Show)

-- NOTE(leana8959): The default mechanism is in field grammar. Nothing is inserted automatically here. Hence is it removed from gpd-barbie branch.

-- TODO(leana8959): Guard MkAnnotated* behind internal / hidden modules.
--                  Then expose smart constructors.
--                  We don't want users to be able to costruct with ExactRepr
--                  We might not be able to prevent user from getting a ExactAnn data,
--                  but we can prevent it from being used.
data Annotated a = MkAnnotated [Comment Position] {- anchor -}Position {- exactrepr -}BS.ByteString (Located a)
  deriving (Show, Functor)

data AnnotatedList a = MkAnnotatedList [Comment Position] {- anchor -}Position {- exactrepr -}BS.ByteString [Located a]
  deriving (Show, Functor)

data Located a = MkLocated { getSrcSpan :: !LocalSrcSpan, unLocated :: !a }
  deriving (Show, Functor)

instance Parsec a => Parsec (Located a) where
  parsec = do
    begin <- asRelativeUnsafe <$> getPosition
    x <- parsec
    end <- asRelativeUnsafe <$> getPosition
    pure (MkLocated (LocalSrcSpan begin end) x)
    where
      -- We know this is relative
      asRelativeUnsafe !(Position r c) = RelPosition r c
