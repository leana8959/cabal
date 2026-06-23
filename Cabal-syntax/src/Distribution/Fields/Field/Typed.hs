module Distribution.Fields.Field.Typed where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Fields.Field ( Name, SectionArg, FieldLine )
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Annotation
import Distribution.Types.Dependency
import Distribution.Version
import Distribution.PackageDescription (LegacyExeDependency(LegacyExeDependency))

data TField ann
  = -- | Holds fields that are not yet transformed to typed implementation.
    MkRawTField !(Name ann) [FieldLine ann]
  | MkCabalVersionTField !(Name ann) (Annotated CabalSpecVersion)
  | MkTargetBuildDependsTField !(Name ann) (AnnotatedList Dependency)
  | MkPkgVersionTField !(Name ann) (Annotated Version)
  | MkBuildToolsTField !(Name ann) (AnnotatedList LegacyExeDependency)
  | MkTSection !(Name ann) [SectionArg ann] [TField ann]
  deriving (Show)
