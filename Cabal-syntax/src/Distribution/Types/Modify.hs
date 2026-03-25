{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Types that can be used as modifiers
module Distribution.Types.Modify where

import Data.Data
import Data.Kind

data Bare = Bare
  deriving (Show, Read, Eq, Ord, Data)

data Ann = Ann
  deriving (Show, Read, Eq, Ord, Data)
