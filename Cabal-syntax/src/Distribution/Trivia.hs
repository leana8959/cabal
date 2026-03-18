{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Trivia
  ( Trivia (..)
  , Trivium (..)
  , Ann (..)
  , mapAnn
  , mapAnnA
  )
  where

import Data.Data
import Data.List.NonEmpty (NonEmpty)

data Trivia
  = HasTrivia (NonEmpty Trivium)
  | ExactRepresentation String
  | IsInserted
  | NoTrivia
  deriving (Show, Eq, Ord, Read, Data)

instance Semigroup Trivia where
  HasTrivia u <> HasTrivia v = HasTrivia (u <> v)

  ExactRepresentation u <> ExactRepresentation v = ExactRepresentation (u <> v)
  u@(ExactRepresentation _) <> _ = u
  _ <> v@(ExactRepresentation _) = v

  NoTrivia <> v = v
  u <> NoTrivia = u

  IsInserted <> _ = IsInserted
  _ <> IsInserted = IsInserted

instance Monoid Trivia where
  mempty = NoTrivia

data Trivium
  = LeadingTrivium String
  | TrailingTrivium String
  deriving (Show, Eq, Ord, Read, Data)

data Ann a = Ann
  { getAnn :: Trivia
  , unAnn :: a
  }
  deriving (Show, Eq, Ord, Functor, Read, Data)


mapAnn
  :: (Trivia -> Trivia)
  -> Ann a
  -> Ann a
mapAnn f (Ann t x) = Ann (f t) x

mapAnnA
  :: (Trivia -> Trivia)
  -> (a -> a)
  -> Ann a
  -> Ann a
mapAnnA f g (Ann t x) = Ann (f t) (g x)
