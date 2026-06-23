module Distribution.Fields.Field.Typed where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Fields.Field ( Name, SectionArg, FieldLine )
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Annotation
import Distribution.Types.Dependency

data TField ann
  = -- | Holds fields that are not yet transformed to typed implementation.
    MkRawTField !(Name ann) [FieldLine ann]
  | MkCabalVersionTField !(Name ann) (Annotated CabalSpecVersion)
  | MkTargetBuildDependsField !(Name ann) (AnnotatedList Dependency)
  | -- | Holds sections that are not yet transformed to typed implementation.
    MkRawTSection !(Name ann) [SectionArg ann] [TField ann]
  deriving (Show)
