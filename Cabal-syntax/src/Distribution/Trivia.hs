module Distribution.Trivia
  ( Trivia (..)
  , Trivium (..)
  , WithTrivia (..)
  )
  where

data Trivia
  = HasTrivia [Trivium]
  | ExactRepresentation String
  | IsInserted
  deriving (Show, Eq, Ord)

data Trivium
  = LeadingTrivium String
  | TrailingTrivium String
  deriving (Show, Eq, Ord)

data WithTrivia a = WithTrivia
  { getTrivia :: Trivia
  , unTrivia :: a
  }
  deriving (Show, Eq, Ord)
