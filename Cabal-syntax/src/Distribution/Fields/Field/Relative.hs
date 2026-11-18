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

-- | As a convention, we say the current position is not yet occupied.
--   Each relative thing should increment the current position to where the next thing can start.
type Relative = RelativeT Identity

type RelativeT (m :: Type -> Type) = StateT Position m

type PosM = State Position

-- TODO(leana8959): the fact that we reuse Position makes it possible to just lift something in the context.
-- Is that good?

data Offset = Offset
      {-# UNPACK #-} !Int -- row offset
      {-# UNPACK #-} !Int -- column offset
  deriving (Eq, Ord, Show, Read)

-- | Position where the line is relative
data RelRowPos = RelRowPos
  {-# UNPACK #-} !Int -- row offset
  {-# UNPACK #-} !Int -- column position

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


{-


-- | The second position's offset to the first one
diffPosition :: Position -> Position -> Offset
diffPosition (Position r0 c0) (Position r1 c1) = Offset (r1 - r0) (c1 - c0)

offsetToPosition :: Position -> Offset -> Position
offsetToPosition (Position r c) (Offset rd cd) = Position (r + rd) (c + cd)

fromRelativeFields :: Position -> Relative [Field ann] -> ([Field ann], Position)
fromRelativeFields = flip runState

toRelativeFields :: Monad m => [Field (WithComments Position)] -> RelativeT m [Field (WithComments Offset)]
toRelativeFields = traverse toRelativeField

-- TODO(leana8959): toRelativeField should be parameterized by the starting position as well

-- | Expects the fields to be sorted in ascending order by 'Position'.
toRelativeField :: Monad m => Field (WithComments Position) -> RelativeT m (Field (WithComments Offset))
toRelativeField (FieldConcrete colonPos fname fls) = do
  fname' <- relativizeName fname
  -- colonPos' <- toOffset colonPos
  fls' <- toRelativeFieldLines fls
  -- TODO(leana8959): colonPos is in correct, however we don't yet have a way to modify the Field datatype to hold either Position or Offset.
  pure (FieldConcrete colonPos fname' fls')
toRelativeField (SectionConcrete sname sargs fs) = do
  sname' <- relativizeName sname
  sargs' <- toRelativeSectionArgs sargs
  fs' <- toRelativeFields fs
  pure (SectionConcrete sname' sargs' fs')

toRelativeAnn :: Monad m => WithComments Position -> RelativeT m (WithComments Offset)
toRelativeAnn (WithComments cmts p) =
  let (pre, post) = partitionCommentsAt p cmts
   in do
        pre' <- toRelativeComments pre
        p' <- toOffset p
        post' <- toRelativeComments post
        pure (WithComments (pre' <> post') p')

toRelativeComments :: Monad m => [Comment Position] -> RelativeT m [Comment Offset]
toRelativeComments = traverse toRelativeComment

toRelativeComment :: Monad m => Comment Position -> RelativeT m (Comment Offset)
toRelativeComment (Comment bs p) = Comment bs <$> toOffset p

toOffset :: Monad m => Position -> RelativeT m Offset
toOffset p = gets (\p0 -> diffPosition p0 p)

partitionCommentsAt :: Position -> [Comment Position] -> ([Comment Position], [Comment Position])
partitionCommentsAt p0 = span (\(Comment _ p) -> p <= p0)

relativizeName :: Monad m => Name (WithComments Position) -> RelativeT m (Name (WithComments Offset))
relativizeName (Name ann name) = Name <$> toRelativeAnn ann <*> pure name

toRelativeFieldLines :: Monad m => [FieldLine (WithComments Position)] -> RelativeT m [FieldLine (WithComments Offset)]
toRelativeFieldLines = traverse toRelativeFieldLine

-- TODO(leana8959): this is wrong. it should update the position with bs
-- TODO(leana8959): check other functions as well.
toRelativeFieldLine :: Monad m => FieldLine (WithComments Position) -> RelativeT m (FieldLine (WithComments Offset))
toRelativeFieldLine (FieldLine ann bs) = FieldLine <$> toRelativeAnn ann <*> pure bs

toRelativeSectionArgs :: Monad m => [SectionArg (WithComments Position)] -> RelativeT m [SectionArg (WithComments Offset)]
toRelativeSectionArgs = traverse toRelativeSectionArg

toRelativeSectionArg :: Monad m => SectionArg (WithComments Position) -> RelativeT m (SectionArg (WithComments Offset))
toRelativeSectionArg (SecArgName ann bs) = SecArgName <$> toRelativeAnn ann <*> pure bs
toRelativeSectionArg (SecArgStr ann bs) = SecArgStr <$> toRelativeAnn ann <*> pure bs
toRelativeSectionArg (SecArgOther ann bs) = SecArgOther <$> toRelativeAnn ann <*> pure bs

modifyRow :: Monad m => Int -> RelativeT m ()
modifyRow n = modify $ \(Position row col) -> Position (row + n) col

modifyCol :: Monad m => Int -> RelativeT m ()
modifyCol n = modify $ \(Position row col) -> Position row (col + n)

-- TODO(leana8959): Implement mkField primitives.
-- It should be possible to build simple fieldlines with strictly incrementing positions.
-- By putting that into a context, it will automatically be relative.
-- This will improve the 'addField' combinator.

mkField
  :: Name ()
  -> [BS.ByteString]
  -- ^ Comments to the 'Name'
  -> [BS.ByteString]
  -- ^ FieldLines
  -> Relative EpField
mkField name cmts fls = do
  cmts' <- mkComments cmts
  name' <- mkRelName name
  colonPos <- offsetBSWithSpace ":" *> get
  fls' <- mkFieldLines fls

  pure (FieldConcrete colonPos (WithComments cmts' <$> name') ((map . fmap) (WithComments []) fls'))

offsetBS :: BS.ByteString -> Relative BS.ByteString
offsetBS bs = bs <$ modify (incPos (BS8.length bs))

-- | Precondition: the string must not be multiline.
--   We put the cursor to the next available character.
offsetBSWithSpace :: BS.ByteString -> Relative BS.ByteString
offsetBSWithSpace bs = bs <$ modify (incPos (BS8.length bs + 1))

mkComments :: [BS.ByteString] -> Relative [Comment Position]
mkComments = traverse mkComment

mkComment :: BS.ByteString -> Relative (Comment Position)
mkComment bs = gets (Comment ("-- " <> bs)) <* modify (incRow 1)

mkRelName :: Name () -> Relative (Name Position)
mkRelName name = gets (<$ name) <* offsetBS (getName name)

mkFieldLines :: [BS.ByteString] -> Relative [FieldLine Position]
mkFieldLines [] = pure @Relative []
mkFieldLines [fl] = fmap (pure @[]) (mkFieldLine fl)
-- TODO(leana8959): parameterize indentation
mkFieldLines fls = indented 2 (traverse mkFieldLine fls)

indented :: Int -> Relative a -> Relative a
indented n r = modify (incPos n) *> r <* modify (incPos (-n))

mkFieldLine :: BS.ByteString -> Relative (FieldLine Position)
mkFieldLine bs = gets (\p -> FieldLine p bs) <* modify (incRow 1)

mkSection
  :: Name ()
  -> [SectionArg ()]
  -> [BS.ByteString]
  -- ^ Comments to the 'Name'
  -> Relative [Field (WithComments Position)]
  -> Relative (Field (WithComments Position))
mkSection sname sargs cmts fs = do
  cmts' <- mkComments cmts
  name' <- mkRelName sname
  sargs' <- mkRelSectionArgs sargs
  fs' <- indented 2 fs
  pure (Section (WithComments cmts' <$> name') ((map . fmap) (WithComments []) sargs') fs')

mkRelSectionArgs :: [SectionArg ()] -> Relative [SectionArg Position]
mkRelSectionArgs = traverse mkRelSectionArg

mkRelSectionArg :: SectionArg () -> Relative (SectionArg Position)
mkRelSectionArg (SecArgName _ bs) = gets (\p -> SecArgName p bs) <* offsetBSWithSpace bs
mkRelSectionArg (SecArgStr _ bs) = gets (\p -> SecArgStr p bs) <* offsetBSWithSpace bs
mkRelSectionArg (SecArgOther _ bs) = gets (\p -> SecArgOther p bs) <* offsetBSWithSpace bs

-}
