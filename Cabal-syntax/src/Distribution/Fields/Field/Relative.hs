{-# LANGUAGE OverloadedStrings #-}

-- | We define an alternative algebra on Fields with relative positioning.
--   This will help the modification, abstracting away the positing shifting.
--
--   The Offset represents the row,column offset to the previous AST element.
module Distribution.Fields.Field.Relative where

import Control.Monad.State (State, StateT, get, gets, modify, runState)
import Distribution.Fields.Field
import Distribution.Parsec.Position

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Identity
import Data.Kind
import Distribution.Compat.Lens (Lens')

-- TODO(leana8959): do the aliases properly

type EpField = FieldConcrete Position (WithComments Position)
type RelEpField = FieldConcrete RelRowPos (WithComments RelRowPos)

type EpFieldLine = FieldLine (WithComments Position)
type RelEpFieldLine = FieldLine (WithComments RelRowPos)

type EpSecArg = SectionArg (WithComments Position)
type RelEpSecArg = SectionArg (WithComments RelRowPos)

type PosM = State Position

-- | Position where the line is relative
data RelRowPos = RelRowPos
  {-# UNPACK #-} !Int -- row offset
  {-# UNPACK #-} !Int -- column position
  deriving (Eq, Ord, Show, Read)

relRowFields :: [EpField] -> PosM [RelEpField]
relRowFields = traverse relRowField

relRowField :: EpField -> PosM RelEpField
relRowField (FieldConcrete colonPos fname fls) = FieldConcrete <$> mkRelRowPos colonPos <*> relRowName fname <*> relRowFieldLines fls
relRowField (SectionConcrete sname sargs fs) = SectionConcrete <$> relRowName sname <*> relRowSecArgs sargs <*> relRowFields fs

relRowName :: Name (WithComments Position) -> PosM (Name (WithComments RelRowPos))
relRowName (Name cmtPos name) = Name <$> relRowCommentsPos cmtPos <*> pure name

relRowFieldLines :: [EpFieldLine] -> PosM [RelEpFieldLine]
relRowFieldLines = traverse relRowFieldLine
relRowFieldLine :: EpFieldLine -> PosM RelEpFieldLine
relRowFieldLine (FieldLine cmtPos bs) = FieldLine <$> relRowCommentsPos cmtPos <*> pure bs


relRowSecArgs :: [EpSecArg] -> PosM [RelEpSecArg]
relRowSecArgs = traverse relRowSecArg
relRowSecArg :: EpSecArg -> PosM RelEpSecArg
relRowSecArg (SecArgName ann bs) = SecArgName <$> relRowCommentsPos ann <*> pure bs
relRowSecArg (SecArgStr ann bs) = SecArgStr <$> relRowCommentsPos ann <*> pure bs
relRowSecArg (SecArgOther ann bs) = SecArgOther <$> relRowCommentsPos ann <*> pure bs

-- basics

relRowCommentsPos :: WithComments Position -> PosM (WithComments RelRowPos)
relRowCommentsPos (WithComments pre post p) = WithComments <$> relRowComments pre <*> relRowComments post <*> mkRelRowPos p

relRowComments :: [Comment Position] -> PosM [Comment RelRowPos]
relRowComments = traverse relRowComment
relRowComment :: Comment Position -> PosM (Comment RelRowPos)
relRowComment (Comment bs pos) = Comment bs <$> mkRelRowPos pos

mkRelRowPos :: Position -> PosM RelRowPos
mkRelRowPos (Position r c) = gets (\(Position r0 _) -> RelRowPos (r - r0) c)

-- TODO(leana8959): Implement mkField primitives that makes relative fields

mkField
  :: Name ()
  -> [BS.ByteString] -- ^ Comments to the 'Name'
  -> [BS.ByteString] -- ^ FieldLines
  -> RelEpField
mkField name cmts fls = undefined

mkSection
  :: Name ()
  -> [SectionArg ()]
  -> [BS.ByteString] -- ^ Comments to the 'Name'
  -> [RelEpField]
  -> (RelEpField)
mkSection sname sargs cmts fs =  undefined
