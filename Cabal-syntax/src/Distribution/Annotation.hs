module Distribution.Annotation where

import qualified Data.ByteString as BS

-- TODO(leana8959): We can label this with a position range so it looks like lsp-style edits, and modifications will mean preforming edits.
data Trivia
  = ExactRepr BS.ByteString
  | IsInserted
  deriving (Show, Eq, Ord, Read)

data Annotated a = MkAnnotated Trivia a
