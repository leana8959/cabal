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
import Distribution.FieldGrammar.Newtypes (SpecVersion, List, CommaVCat)
import Distribution.FieldGrammar.Parsec (runFieldParser, fieldLinesToBS, getFieldLinesFirstAnn)
import Distribution.Parsec.Position
import Distribution.Annotation
import Distribution.Compat.Newtype (Newtype(unpack))
import Distribution.Types.Dependency
import Distribution.Version
import Distribution.PackageDescription (LegacyExeDependency)

typeFields :: CabalSpecVersion -> [Field (WithComments Position)] -> ParseResult src [TField (WithComments Position)]
typeFields = traverse . typeField

typeField :: CabalSpecVersion -> Field (WithComments Position) -> ParseResult src (TField (WithComments Position))
typeField csv (Field fname fls)
  -- example for single value
  | getName fname == "cabal-version" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    let fnamePos = unComments $ nameAnn fname
    let anc = unComments <$> getFieldLinesFirstAnn fls
    lsv <- fmap unpack <$> runFieldParser fnamePos (parsec @(Located SpecVersion)) csv fls'
    pure (MkCabalVersionTField fname (Annotate cmts anc (fieldLinesToBS fls) lsv))

  | getName fname == "version" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    let fnamePos = unComments $ nameAnn fname
    let anc = unComments <$> getFieldLinesFirstAnn fls
    lv <- runFieldParser fnamePos (parsec @(Located Version)) csv fls'
    pure (MkPkgVersionTField fname (Annotate cmts anc (fieldLinesToBS fls) lv))

  -- example for many values
  | getName fname == "build-depends" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    let parseListDeps :: CabalParsing m => m (List CommaVCat (Identity (Located Dependency)) (Located Dependency))
        parseListDeps = parsec
        parseDeps :: CabalParsing m => m [Located Dependency]
        parseDeps = unpack <$> parseListDeps
    let fnamePos = unComments $ nameAnn fname
    let anc = unComments <$> getFieldLinesFirstAnn fls
    deps <- runFieldParser fnamePos parseDeps csv fls'
    let deps' = AnnotateList cmts anc (fieldLinesToBS fls) deps
    pure (MkTargetBuildDependsTField fname deps')

  -- example for many values
  | getName fname == "build-tools" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    let parseListBuildTools :: CabalParsing m => m (List CommaVCat (Identity (Located LegacyExeDependency)) (Located LegacyExeDependency))
        parseListBuildTools = parsec
        parseBuildTools :: CabalParsing m => m [Located LegacyExeDependency]
        parseBuildTools = unpack <$> parseListBuildTools
    let fnamePos = unComments $ nameAnn fname
    let anc = unComments <$> getFieldLinesFirstAnn fls
    bts <- runFieldParser fnamePos parseBuildTools csv fls'
    let bts' = AnnotateList cmts anc (fieldLinesToBS fls) bts
    pure (MkBuildToolsTField fname bts')

  -- store all legacy value in a fourre-tout
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
