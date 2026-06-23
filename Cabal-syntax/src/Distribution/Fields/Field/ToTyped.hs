{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Fields.Field.ToTyped where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Fields.Field
import Distribution.Fields.Field.Typed
import Distribution.Fields.ParseResult ( ParseResult )
import qualified Data.Bifunctor as Bi

import Distribution.CabalSpecVersion
import Distribution.Parsec (Parsec(parsec))
import Distribution.FieldGrammar.Newtypes (SpecVersion)
import Distribution.FieldGrammar.Parsec (runFieldParser, fieldLinesToBS, fieldLinesToSrcSpan)
import Distribution.Parsec.Position
import Distribution.Annotation
import Distribution.Compat.Newtype (Newtype(unpack))

typeFields :: CabalSpecVersion -> [Field (WithComments Position)] -> ParseResult src [TField (WithComments Position)]
typeFields = traverse . typeField

typeField :: CabalSpecVersion -> Field (WithComments Position) -> ParseResult src (TField (WithComments Position))
typeField csv (Field fname fls)
  | getName fname == "cabal-version" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    sv <- unpack <$> runFieldParser (unComments $ nameAnn fname) (parsec @SpecVersion) csv fls'
    let spn = fieldLinesToSrcSpan fls'
    let ann = ExactRepr spn (fieldLinesToBS fls)
    pure (MkCabalVersionTField fname (MkAnnotated cmts ann sv))
  | otherwise = pure (MkRawTField fname fls)
typeField csv (Section sname sargs fs) = do
  tfs <- typeFields csv fs
  pure (MkRawTSection sname sargs tfs)

extractCommentsFieldLines :: [FieldLine (WithComments Position)] -> ([Comment Position], [FieldLine Position])
extractCommentsFieldLines = Bi.first mconcat . unzip . map extractCommentsFieldLine

extractCommentsFieldLine :: FieldLine (WithComments Position) -> ([Comment Position], FieldLine Position)
extractCommentsFieldLine (FieldLine ann bs) =
  let WithComments cmts pos = ann
  in  (cmts, FieldLine pos bs)
