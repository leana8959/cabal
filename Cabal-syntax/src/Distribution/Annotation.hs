{-# LANGUAGE DeriveFunctor #-}

module Distribution.Annotation where

import qualified Data.ByteString as BS
import Distribution.Parsec.Position
import Distribution.Fields.Field

import Distribution.Parsec

data SrcSpan = MkSrcSpan {-# UNPACK #-} !Position {-# UNPACK #-} !Position
  deriving (Show)

-- NOTE(leana8959): The default mechanism is in field grammar. Nothing is inserted automatically here. Hence is it removed from gpd-barbie branch.

-- TODO(leana8959): Guard MkAnnotated* behind internal / hidden modules.
--                  Then expose smart constructors.
--                  We don't want users to be able to costruct with ExactRepr
--                  We might not be able to prevent user from getting a ExactAnn data,
--                  but we can prevent it from being used.
data Annotated a = MkAnnotated [Comment Position] BS.ByteString (Located a)
  deriving (Show, Functor)

data AnnotatedList a = MkAnnotatedList [Comment Position] BS.ByteString [Located a]
  deriving (Show, Functor)

data Located a = MkLocated { getSrcSpan :: !SrcSpan, unLocated :: !a }
  deriving (Show, Functor)

instance Parsec a => Parsec (Located a) where
  parsec = do
    begin <- getPosition
    x <- parsec
    end <- getPosition
    pure (MkLocated (MkSrcSpan begin end) x)
