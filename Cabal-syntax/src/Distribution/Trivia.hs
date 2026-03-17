module Distribution.Trivia
  ( Trivia (..)
  , Trivium (..)
  , WithTrivia (..)
  )
  where

data Trivia
  = HasTrivia [Trivium]
  | ExactRepresentation
  | IsInserted

data Trivium
  = LeadingTrivium String
  | TrailingTrivium String

data WithTrivia a = WithTrivia
  { getTrivia :: Trivia
  , unTrivia :: a
  }
