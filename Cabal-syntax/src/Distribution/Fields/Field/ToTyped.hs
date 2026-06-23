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
import Distribution.FieldGrammar.Parsec (runFieldParser, fieldLinesToBS)
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
    lsv <- fmap unpack <$> runFieldParser (unComments $ nameAnn fname) (parsec @(Located SpecVersion)) csv fls'
    let ann = ExactRepr (fieldLinesToBS fls)
    pure (MkCabalVersionTField fname (MkAnnotated cmts ann lsv))

  -- example for many values
  | getName fname == "build-depends" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    let parseListDeps :: CabalParsing m => m (List CommaVCat (Identity (Located Dependency)) (Located Dependency))
        parseListDeps = parsec
        parseDeps :: CabalParsing m => m [Located Dependency]
        parseDeps = unpack <$> parseListDeps
    let eann = ExactRepr (fieldLinesToBS fls)
    deps <- runFieldParser (unComments $ nameAnn fname) parseDeps csv fls'
    let deps' = MkAnnotatedList cmts eann deps
    pure (MkTargetBuildDependsTField fname deps')

  | getName fname == "version" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    lv <- runFieldParser (unComments $ nameAnn fname) (parsec @(Located Version)) csv fls'
    let eann = ExactRepr (fieldLinesToBS fls)
    pure (MkPkgVersionTField fname (MkAnnotated cmts eann lv))

  -- example for many values
  | getName fname == "build-tools" = do
    let (cmts, fls') = extractCommentsFieldLines fls
    let parseListBuildTools :: CabalParsing m => m (List CommaVCat (Identity (Located LegacyExeDependency)) (Located LegacyExeDependency))
        parseListBuildTools = parsec
        parseBuildTools :: CabalParsing m => m [Located LegacyExeDependency]
        parseBuildTools = unpack <$> parseListBuildTools
    let eann = ExactRepr (fieldLinesToBS fls)
    bts <- runFieldParser (unComments $ nameAnn fname) parseBuildTools csv fls'
    let bts' = MkAnnotatedList cmts eann bts
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
