{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Types that can be used as modifiers
module Distribution.Types.Modify where

import Data.Data

-- | Toggle whether a GPD component has annotation or not.
data HasAnnotation
  = HasAnn
  | HasNoAnn
  deriving (Show, Read, Eq, Ord, Data)

-- | Toggle whether a Pretty instance should in position in its output.
data HasPosition
  = HasPos
  | HasNoPos
