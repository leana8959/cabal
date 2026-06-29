module Distribution.Fields.Field.FromTyped where

import Distribution.Fields.Field.Typed
import Distribution.CabalSpecVersion

class FromTField a where
  fromTField :: CabalSpecVersion -> TField ann -> Maybe a
