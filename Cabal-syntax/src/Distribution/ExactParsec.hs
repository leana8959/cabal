module Distribution.ExactParsec
  ( ExactParsec(..)
  )
  where

import Distribution.Parsec
import Distribution.Trivia

class ExactParsec a where
  exactParsec :: CabalParsing m => m (WithTrivia a)
