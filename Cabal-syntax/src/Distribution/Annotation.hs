module Distribution.Annotation where

import qualified Data.ByteString as BS
import Distribution.Parsec.Position
import Distribution.Fields.Field


-- TODO(leana8959): We can label this with a position range so it looks like lsp-style edits, and modifications will mean preforming edits.
data ExactAnn
  = ExactRepr (Maybe SrcSpan) BS.ByteString
  | IsInserted
  deriving (Show)

data SrcSpan = MkSrcSpan {-# UNPACK #-} !Position {-# UNPACK #-} !Position
  deriving (Show)

data Annotated a = MkAnnotated [Comment Position] ExactAnn a
  deriving (Show)
