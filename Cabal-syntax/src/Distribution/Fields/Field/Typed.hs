{-# LANGUAGE LambdaCase #-}
module Distribution.Fields.Field.Typed where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Fields.Field ( Name, SectionArg, FieldLine , FieldName, getName )
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

getTFieldName :: TField ann -> Name ann
getTFieldName (MkRawTField name _) = name
getTFieldName (MkCabalVersionTField name _) = name
getTFieldName (MkTargetBuildDependsTField name _) = name
getTFieldName (MkPkgVersionTField name _) = name
getTFieldName (MkBuildToolsTField name _) = name
getTFieldName (MkTSection {}) = error "getTFieldName: expecting a field, got a section"

-- I don't have a better solution than to use partial functions in field grammar at the moment.
getTFieldByName :: FieldName -> [TField ann] -> [TField ann]
getTFieldByName name0 = mapMaybe go
  where
    go tfield@(MkRawTField name _) | getName name == name0 = Just tfield
    go tfield@(MkCabalVersionTField name _) | getName name == name0 = Just tfield
    go tfield@(MkTargetBuildDependsTField name _) | getName name == name0 = Just tfield
    go tfield@(MkPkgVersionTField name _) | getName name == name0 = Just tfield
    go tfield@(MkBuildToolsTField name _) | getName name == name0 = Just tfield
    go (MkTSection {}) = error "getTFieldName: expecting a field, got a section"
    go _ = Nothing

-- getCabalVersionField :: [TField ann] -> [(Name ann, Annotated CabalSpecVersion)]
-- getCabalVersionField = mapMaybe $ \case { (MkCabalVersionTField name x) -> Just (name, x); _ -> Nothing; }
--
-- getTargetBuildDependsField :: [TField ann] -> [(Name ann, AnnotatedList Dependency)]
-- getTargetBuildDependsField = mapMaybe $ \case { (MkTargetBuildDependsTField name x) -> Just (name, x); _ -> Nothing; }
--
-- getPkgVersionField :: [TField ann] -> [(Name ann, Annotated Version)]
-- getPkgVersionField = mapMaybe $ \case { (MkPkgVersionTField name x) -> Just (name, x); _ -> Nothing; }
--
-- getBuildToolsField :: [TField ann] -> [(Name ann, AnnotatedList LegacyExeDependency)]
-- getBuildToolsField = mapMaybe $ \case { (MkBuildToolsTField name x) -> Just (name, x); _ -> Nothing; }
