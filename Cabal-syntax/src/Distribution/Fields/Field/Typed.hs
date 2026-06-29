{-# LANGUAGE LambdaCase #-}
module Distribution.Fields.Field.Typed where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Fields.Field ( Name, SectionArg, FieldLine )
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Annotation
import Distribution.Types.Dependency
import Distribution.Version
import Distribution.PackageDescription (LegacyExeDependency)

data TField ann
  = -- | Holds fields that are not yet transformed to typed implementation.
    MkRawTField !(Name ann) [FieldLine ann]
  | MkCabalVersionTField !(Name ann) (Annotated CabalSpecVersion)
  | MkTargetBuildDependsTField !(Name ann) (AnnotatedList Dependency)
  | MkPkgVersionTField !(Name ann) (Annotated Version)
  | MkBuildToolsTField !(Name ann) (AnnotatedList LegacyExeDependency)
  | MkTSection !(Name ann) [SectionArg ann] [TField ann]
  deriving (Show)

getCabalVersionField :: [TField ann] -> [(Name ann, Annotated CabalSpecVersion)]
getCabalVersionField = mapMaybe $ \case { (MkCabalVersionTField name x) -> Just (name, x); _ -> Nothing; }

getTargetBuildDependsField :: [TField ann] -> [(Name ann, AnnotatedList Dependency)]
getTargetBuildDependsField = mapMaybe $ \case { (MkTargetBuildDependsTField name x) -> Just (name, x); _ -> Nothing; }

getPkgVersionField :: [TField ann] -> [(Name ann, Annotated Version)]
getPkgVersionField = mapMaybe $ \case { (MkPkgVersionTField name x) -> Just (name, x); _ -> Nothing; }

getBuildToolsField :: [TField ann] -> [(Name ann, AnnotatedList LegacyExeDependency)]
getBuildToolsField = mapMaybe $ \case { (MkBuildToolsTField name x) -> Just (name, x); _ -> Nothing; }
