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
import Distribution.Parsec (Parsec(parsec), CabalParsing)
import Distribution.FieldGrammar.Newtypes (SpecVersion, List, CommaVCat, LocatedP (..))
import Distribution.FieldGrammar.Parsec (runFieldParser, fieldLinesToBS, fieldLinesToSrcSpan)
import Distribution.Parsec.Position
import Distribution.Annotation
import Distribution.Compat.Newtype (Newtype(unpack))
import Distribution.Types.Dependency 

typeFields :: CabalSpecVersion -> [Field (WithComments Position)] -> ParseResult src [TField (WithComments Position)]
typeFields = traverse . typeField

typeField :: CabalSpecVersion -> Field (WithComments Position) -> ParseResult src (TField (WithComments Position))
typeField csv (Field fname fls)
  | getName fname == "cabal-version" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    sv <- unpack <$> runFieldParser (unComments $ nameAnn fname) (parsec @SpecVersion) csv fls'
    let spn = fieldLinesToSrcSpan fls'
    let ann = ExactRepr (fieldLinesToBS fls)
    pure (MkCabalVersionTField fname (MkAnnotated cmts ann spn sv))

  | getName fname == "build-depends" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    let parseListDeps :: CabalParsing m => m (List CommaVCat (Identity (LocatedP Dependency)) (LocatedP Dependency))
        parseListDeps = parsec
        parseDeps :: CabalParsing m => m [LocatedP Dependency]
        parseDeps = unpack <$> parseListDeps
    let eann = ExactRepr (fieldLinesToBS fls)
    deps <- runFieldParser (unComments $ nameAnn fname) parseDeps csv fls'

    let deps' = map (\(MkLocatedP spn x) -> (spn, x)) deps
    let deps'' = MkAnnotatedList cmts eann deps'
    pure (MkTargetBuildDependsField fname deps'')

  | otherwise = pure (MkRawTField fname fls)
typeField csv (Section sname sargs fs) = do
  tfs <- typeFields csv fs
  pure (MkTSection sname sargs tfs)

extractCommentsFieldLines :: [FieldLine (WithComments Position)] -> ([Comment Position], [FieldLine Position])
extractCommentsFieldLines = Bi.first mconcat . unzip . map extractCommentsFieldLine

extractCommentsFieldLine :: FieldLine (WithComments Position) -> ([Comment Position], FieldLine Position)
extractCommentsFieldLine (FieldLine ann bs) =
  let WithComments cmts pos = ann
  in  (cmts, FieldLine pos bs)
