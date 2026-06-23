module Distribution.Fields.Field.Typed where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Fields.Field ( Name, SectionArg, FieldLine )
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Annotation

data TField ann
  = -- | Holds fields that are not yet transformed to typed implementation.
    MkRawTField !(Name ann) [FieldLine ann]
  | MkCabalVersionTField !(Name ann) (Annotated CabalSpecVersion)
  | -- | Holds sections that are not yet transformed to typed implementation.
    MkRawTSection !(Name ann) [SectionArg ann] [TField ann]
  deriving (Show)
