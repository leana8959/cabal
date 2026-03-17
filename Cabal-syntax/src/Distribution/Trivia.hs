{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Trivia
  ( Trivia (..)
  , Trivium (..)
  , WithTrivia (..)
  )
  where

import Data.Data

data Trivia
  = HasTrivia [Trivium]
  | ExactRepresentation String
  | IsInserted
  deriving (Show, Eq, Ord, Read, Data)

data Trivium
  = LeadingTrivium String
  | TrailingTrivium String
  deriving (Show, Eq, Ord, Read, Data)

data WithTrivia a = WithTrivia
  { getTrivia :: Trivia
  , unTrivia :: a
  }
  deriving (Show, Eq, Ord, Functor, Read, Data)
