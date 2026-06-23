module Distribution.Annotation where

import qualified Data.ByteString as BS
import Distribution.Parsec.Position
import Distribution.Fields.Field


-- TODO(leana8959): We can label this with a position range so it looks like lsp-style edits, and modifications will mean preforming edits.
data ExactAnn
  = ExactRepr BS.ByteString
  | IsInserted
  deriving (Show)

data SrcSpan = MkSrcSpan {-# UNPACK #-} !Position {-# UNPACK #-} !Position
  deriving (Show)

-- TODO(leana8959): refine type
data Annotated a = MkAnnotated [Comment Position] ExactAnn (Maybe SrcSpan) a
  deriving (Show)

-- NOTE(leana8959): Move Located to its own module, here's a cycle
data AnnotatedList a = MkAnnotatedList [Comment Position] ExactAnn [(SrcSpan, a)]
  deriving (Show)
