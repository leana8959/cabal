{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
module Distribution.Fields.Field.Typed where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Fields.Field ( Name, Name', SectionArg, FieldLine , FieldName, getName )
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Annotation
import Distribution.Types.Dependency
import Distribution.Version
import Distribution.PackageDescription (LegacyExeDependency)

data FieldKind a where
  CabalSpecVersionField :: FieldKind (Annotated CabalSpecVersion)
  TargetBuildDependsField :: FieldKind (AnnotatedList Dependency)

deriving instance Show a => Show (FieldKind a)

data TField ann
  = -- | Holds fields that are not yet transformed to typed implementation.
    MkRawTField !(Name ann) [FieldLine ann]
  | forall a. (Show a, Eq a) => MkTField (FieldKind a) (Name ann) a
  | MkCabalVersionTField !(Name ann) (Annotated CabalSpecVersion)
  | MkTargetBuildDependsTField !(Name ann) (AnnotatedList Dependency)
  | MkPkgVersionTField !(Name ann) (Annotated Version)
  | MkBuildToolsTField !(Name ann) (AnnotatedList LegacyExeDependency)
  | MkTSection !(Name ann) [SectionArg ann] [TField ann]

deriving instance Show a => Show (TField a)

getTFieldName :: TField ann -> Name ann
getTFieldName (MkRawTField name _) = name
getTFieldName (MkCabalVersionTField name _) = name
getTFieldName (MkTargetBuildDependsTField name _) = name
getTFieldName (MkPkgVersionTField name _) = name
getTFieldName (MkBuildToolsTField name _) = name
getTFieldName (MkTSection {}) = error "getTFieldName: expecting a field, got a section"

-- I don't have a better solution than to use partial functions in field grammar at the moment.
getTFieldByName :: FieldName -> [TField ann] -> [TField ann]
getTFieldByName name = getTFieldByNameP (name ==)

getTFieldByNameP :: (FieldName -> Bool) -> [TField ann] -> [TField ann]
getTFieldByNameP nameP = mapMaybe go
  where
    go tfield@(MkRawTField name _) | nameP (getName name) = Just tfield
    go tfield@(MkCabalVersionTField name _) | nameP (getName name) = Just tfield
    go tfield@(MkTargetBuildDependsTField name _) | nameP (getName name) = Just tfield
    go tfield@(MkPkgVersionTField name _) | nameP (getName name) = Just tfield
    go tfield@(MkBuildToolsTField name _) | nameP (getName name) = Just tfield
    go (MkTSection {}) = error "getTFieldName: expecting a field, got a section"
    go _ = Nothing

-- TODO: return maybe
getValue :: ( forall a. FieldKind a -> Name ann -> a -> r ) -> TField ann -> r
getValue useValue (MkTField fk fname x) = useValue fk fname x
