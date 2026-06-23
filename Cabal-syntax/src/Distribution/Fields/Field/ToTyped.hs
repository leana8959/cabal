{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Fields.Field.ToTyped where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Fields.Field
import Distribution.Fields.Field.Typed
import Distribution.Fields.ParseResult ( ParseResult )

import Distribution.CabalSpecVersion
import Distribution.Parsec (Parsec(parsec))
import Distribution.FieldGrammar.Newtypes (SpecVersion)
import Distribution.FieldGrammar.Parsec (runFieldParser, fieldLinesToBS, fieldLinesToSrcSpan)
import Distribution.Parsec.Position
import Distribution.Annotation
import Distribution.Compat.Newtype (Newtype(unpack))

typeFields :: CabalSpecVersion -> [Field Position] -> ParseResult src [TField Position]
typeFields = traverse . typeField

typeField :: CabalSpecVersion -> Field Position -> ParseResult src (TField Position)
typeField csv (Field fname fls)
  | getName fname == "cabal-version" = do
    sv <- unpack <$> runFieldParser (nameAnn fname) (parsec @SpecVersion) csv fls
    let spn = fieldLinesToSrcSpan fls
    let ann = ExactRepr spn (fieldLinesToBS fls)
    pure (MkCabalVersionTField fname (MkAnnotated ann sv))
  | otherwise = pure (MkRawTField fname fls)
typeField csv (Section sname sargs fs) = do
  tfs <- typeFields csv fs
  pure (MkRawTSection sname sargs tfs)
