{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Typed Fields -> GPD
module Distribution.FieldGrammar.TParsec
  ( TParsecFieldGrammar
  , parseTFieldGrammar
  , parseTFieldGrammarCheckingStanzas
  , tFieldGrammarKnownFieldList

    -- * Auxiliary
  , Fields
  , NamelessField (..)
  , namelessFieldAnn
  , Section (..)
  , runFieldParser
  , runFieldParser'
  , fieldLinesToStream
  , fieldLinesToBS
  , getFieldLinesFirstAnn
  , freeTextIgnoreDotlineVers
  ) where

import Distribution.Compat.Newtype
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.Utils.String (trim)
import Prelude ()

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Utils.ShortText as ShortText
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P

import Distribution.CabalSpecVersion
import Distribution.FieldGrammar.Class
import Distribution.Fields.Field
import Distribution.Fields.Field.Typed
import Distribution.Fields.Field.ToTyped
import Distribution.Fields.Field.FromTyped
import Distribution.Fields.ParseResult
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream
import Distribution.Parsec.Position (positionCol, positionRow)
import Distribution.Utils.Generic

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

type Fields ann = Map FieldName [NamelessField ann]

-- | Single field, without name, but with its annotation.
data NamelessField ann = MkNamelessField !ann [FieldLine ann]
  deriving (Eq, Show, Functor)

namelessFieldAnn :: NamelessField ann -> ann
namelessFieldAnn (MkNamelessField ann _) = ann

-- | The 'Section' constructor of 'Field'.
data Section ann = MkSection !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor)

-------------------------------------------------------------------------------
-- ParsecFieldGrammar
-------------------------------------------------------------------------------

data TParsecFieldGrammar s a = TParsecFG
  { tFieldGrammarKnownFields :: !(Set FieldName)
  , tFieldGrammarKnownPrefixes :: !(Set FieldName)
  , tFieldGrammarParser :: forall src. (CabalSpecVersion -> [TField (WithComments Position)] -> ParseResult src a)
  }
  deriving (Functor)

-- TODO(leana8959): restore warning for each fieldline
parseTFieldGrammar :: CabalSpecVersion -> [TField (WithComments Position)] -> TParsecFieldGrammar s a -> ParseResult src a
parseTFieldGrammar v tfields grammar = do
  for_ (filter (isUnknownTField grammar) tfields) $ \(getTFieldName->Name ann unknownTFieldName) ->
    parseWarning (unComments ann) PWTUnknownField $ "Unknown field: " ++ show unknownTFieldName

  -- parse
  tFieldGrammarParser grammar v tfields

isUnknownTField :: TParsecFieldGrammar s a -> TField ann -> Bool
isUnknownTField grammar tfield =
  let fname = getName $ getTFieldName tfield
  in  not $
          fname `Set.member` tFieldGrammarKnownFields grammar
            || any (`BS.isPrefixOf` fname) (tFieldGrammarKnownPrefixes grammar)

-- | Parse a ParsecFieldGrammar and check for fields that should be stanzas.
parseTFieldGrammarCheckingStanzas :: CabalSpecVersion -> [TField (WithComments Position)] -> TParsecFieldGrammar s a -> Set BS.ByteString -> ParseResult src a
parseTFieldGrammarCheckingStanzas v tfields grammar sections = do
  for_ (filter (isUnknownTField grammar) tfields) $ \(getTFieldName->Name ann unknownTFieldName) ->
    let pos = unComments ann
    in  if unknownTFieldName `Set.member` sections
          then parseFailure pos $ "'" ++ fromUTF8BS unknownTFieldName ++ "' is a stanza, not a field. Remove the trailing ':' to parse a stanza."
          else parseWarning pos PWTUnknownField $ "Unknown field: " ++ show unknownTFieldName
  tFieldGrammarParser grammar v tfields

tFieldGrammarKnownFieldList :: TParsecFieldGrammar s a -> [FieldName]
tFieldGrammarKnownFieldList = Set.toList . tFieldGrammarKnownFields

instance Applicative (TParsecFieldGrammar s) where
  pure x = TParsecFG mempty mempty (\_ _ -> pure x)
  {-# INLINE pure #-}

  TParsecFG f f' f'' <*> TParsecFG x x' x'' =
    TParsecFG
      (f <> x)
      (f' <> x')
      (\v fields -> f'' v fields <*> x'' v fields)
  {-# INLINE (<*>) #-}

warnMultipleSingularFields :: FieldName -> [NamelessField Position] -> ParseResult src ()
warnMultipleSingularFields _ [] = pure ()
warnMultipleSingularFields fn (x : xs) = do
  let pos = namelessFieldAnn x
      poss = map namelessFieldAnn xs
  parseWarning pos PWTMultipleSingularField $
    "The field " <> show fn <> " is specified more than once at positions " ++ intercalate ", " (map showPos (pos : poss))

instance FieldGrammar FromTField TParsecFieldGrammar where
  blurFieldGrammar _ (TParsecFG s s' parser) = TParsecFG s s' parser

  uniqueFieldAla fn _pack _extract = TParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields =
        case mapMaybe (fromTField v) fields of
          [] -> parseFatalFailure zeroPos $ show fn ++ " field missing"
          [x] -> pure $ unpack' _pack x
          xs@(_ : y : ys) -> do
           -- warnMultipleSingularFields fn xs
           pure $ NE.last $ fmap (unpack' _pack) $ y :| ys

  booleanFieldDef fn _extract def = TParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case mapMaybe (fromTField v) fields of
        [] -> pure def
        [v] -> pure v
        xs@(_ : y : ys) -> do
          -- warnMultipleSingularFields fn xs
          pure $ NE.last $ y :| ys

  optionalFieldAla fn _pack _extract = TParsecFG (Set.singleton fn) Set.empty parser
    where
      -- TODO(leana8959): we need to detect whether the fieldlines are empty before running the parser
      parser v fields = case mapMaybe (fromTField v) fields of
        [] -> pure Nothing
        [v] -> pure $ Just $ unpack' _pack v
        xs@(_ : y : ys) -> do
          -- warnMultipleSingularFields fn xs
          pure $ Just $ NE.last $ fmap (unpack' _pack) $ y :| ys

  optionalFieldDefAla fn _pack _extract def = TParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case mapMaybe (fromTField v) fields of
        [] -> pure def
        [v] -> pure $ unpack' _pack $ v
        xs@(_ : y : ys) -> do
          -- warnMultipleSingularFields fn xs
          pure $ unpack' _pack $ NE.last $ y :| ys

  freeTextField fn _ = TParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case mapMaybe (fromTField v) fields of
        [] -> pure Nothing
        [x] -> pure $ Just x
        xs@(_ : y : ys) -> do
          -- warnMultipleSingularFields fn xs
          pure $ Just $ NE.last $ y :| ys

  freeTextFieldDef fn _ = TParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case mapMaybe (fromTField v) fields of
        [] -> pure ""
        [x] -> pure x
        xs@(_ : y : ys) -> do
          -- warnMultipleSingularFields fn xs
          pure $ NE.last $ y :| ys

  -- freeTextFieldDefST = defaultFreeTextFieldDefST
  freeTextFieldDefST fn _ = TParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case mapMaybe (fromTField v) fields of
        [] -> pure mempty
        [x] -> pure x
        xs@(_ : y : ys) -> do
          -- warnMultipleSingularFields fn xs
          pure $ NE.last $ y :| ys

  monoidalFieldAla fn _pack _extract = TParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case mapMaybe (fromTField v) fields of
        [] -> pure mempty
        xs -> pure $ mconcat $ map (unpack' _pack) $ xs

  -- TODO(leana8959):
  prefixedFields = undefined
  -- prefixedFields fnPfx _extract = TParsecFG mempty (Set.singleton fnPfx) (\_ fs -> pure (parser fs))
  --   where
  --     parser :: Fields Position -> [(String, String)]
  --     parser values = reorder $ concatMap convert $ filter match $ Map.toList values

  --     match (fn, _) = fnPfx `BS.isPrefixOf` fn
  --     convert (fn, fields) =
  --       [ (pos, (fromUTF8BS fn, trim $ fromUTF8BS $ fieldlinesToBS fls))
  --       | MkNamelessField pos fls <- fields
  --       ]
  --     -- hack: recover the order of prefixed fields
  --     reorder = map snd . sortBy (comparing fst)

  availableSince = undefined
  -- availableSince vs def (TParsecFG names prefixes parser) = TParsecFG names prefixes parser'
  --   where
  --     parser' v values
  --       | v >= vs = parser v values
  --       | otherwise = do
  --           let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
  --           for_ (Map.toList unknownFields) $ \(name, fields) ->
  --             for_ fields $ \(MkNamelessField pos _) ->
  --               parseWarning pos PWTUnknownField $
  --                 "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ ". This field will be ignored."

  --           pure def

  availableSinceWarn = undefined
  -- availableSinceWarn vs (TParsecFG names prefixes parser) = TParsecFG names prefixes parser'
  --   where
  --     parser' v values
  --       | v >= vs = parser v values
  --       | otherwise = do
  --           let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
  --           for_ (Map.toList unknownFields) $ \(name, fields) ->
  --             for_ fields $ \(MkNamelessField pos _) ->
  --               parseWarning pos PWTUnknownField $
  --                 "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ "."

  --           parser v values

  deprecatedSince = undefined
  -- -- todo we know about this field
  -- deprecatedSince vs msg (TParsecFG names prefixes parser) = TParsecFG names prefixes parser'
  --   where
  --     parser' v values
  --       | v >= vs = do
  --           let deprecatedFields = Map.intersection values $ Map.fromSet (const ()) names
  --           for_ (Map.toList deprecatedFields) $ \(name, fields) ->
  --             for_ fields $ \(MkNamelessField pos _) ->
  --               parseWarning pos PWTDeprecatedField $
  --                 "The field " <> show name <> " is deprecated in the Cabal specification version " ++ showCabalSpecVersion vs ++ ". " ++ msg

  --           parser v values
  --       | otherwise = parser v values

  removedIn = undefined
  -- removedIn vs msg (TParsecFG names prefixes parser) = TParsecFG names prefixes parser'
  --   where
  --     parser' v values
  --       | v >= vs = do
  --           let msg' = if null msg then "" else ' ' : msg
  --           let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
  --           let namePos =
  --                 [ (name, pos)
  --                 | (name, fields) <- Map.toList unknownFields
  --                 , MkNamelessField pos _ <- fields
  --                 ]

  --           let makeMsg name = "The field " <> show name <> " is removed in the Cabal specification version " ++ showCabalSpecVersion vs ++ "." ++ msg'

  --           case namePos of
  --             -- no fields => proceed (with empty values, to be sure)
  --             [] -> parser v mempty
  --             -- if there's single field: fail fatally with it
  --             ((name, pos) : rest) -> do
  --               for_ rest $ \(name', pos') -> parseFailure pos' $ makeMsg name'
  --               parseFatalFailure pos $ makeMsg name
  --       | otherwise = parser v values

  knownField = undefined
  -- knownField fn = TParsecFG (Set.singleton fn) Set.empty (\_ _ -> pure ())

  hiddenField = id

-------------------------------------------------------------------------------
-- Parsec
-------------------------------------------------------------------------------

runFieldParser' :: [Position] -> ParsecParser a -> CabalSpecVersion -> FieldLineStream -> ParseResult src a
runFieldParser' inputPoss p v str = case P.runParser p' [] "<field>" str of
  Right (pok, ws) -> do
    traverse_ (\(PWarning t pos w) -> parseWarning (mapPosition pos) t w) ws
    pure pok
  Left err -> do
    let ppos = P.errorPos err
    let epos = mapPosition $ Position (P.sourceLine ppos) (P.sourceColumn ppos)

    let msg =
          P.showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            (P.errorMessages err)
    parseFatalFailure epos $ msg ++ "\n"
  where
    p' = (,) <$ P.spaces <*> unPP p v <* P.spaces <* P.eof <*> P.getState

    -- Positions start from 1:1, not 0:0
    mapPosition (Position prow pcol) = go (prow - 1) inputPoss
      where
        go _ [] = zeroPos
        go _ [Position row col] = Position row (col + pcol - 1)
        go n (Position row col : _) | n <= 0 = Position row (col + pcol - 1)
        go n (_ : ps) = go (n - 1) ps

runFieldParser :: Position -> ParsecParser a -> CabalSpecVersion -> [FieldLine Position] -> ParseResult src a
runFieldParser pp p v ls = runFieldParser' poss p v (fieldLinesToStream ls)
  where
    poss = map (\(FieldLine pos _) -> pos) ls ++ [pp] -- add "default" position

fieldlinesToBS :: [FieldLine ann] -> BS.ByteString
fieldlinesToBS = BS.intercalate "\n" . map (\(FieldLine _ bs) -> bs)

-- Example package with dot lines
-- http://hackage.haskell.org/package/copilot-cbmc-0.1/copilot-cbmc.cabal
fieldlinesToFreeText :: [FieldLine ann] -> String
fieldlinesToFreeText [FieldLine _ "."] = "."
fieldlinesToFreeText fls = intercalate "\n" (map go fls)
  where
    go (FieldLine _ bs)
      | s == "." = ""
      | otherwise = s
      where
        s = trim (fromUTF8BS bs)

-- | Cabal version where we switch from the old free text parser that had
-- special logic for "dotlines" to a new parser that has no such logic.
freeTextIgnoreDotlineVers :: CabalSpecVersion
freeTextIgnoreDotlineVers = CabalSpecV3_0

fieldlinesToFreeText3 :: Position -> [FieldLine Position] -> String
fieldlinesToFreeText3 _ [] = ""
fieldlinesToFreeText3 _ [FieldLine _ bs] = fromUTF8BS bs
fieldlinesToFreeText3 pos (FieldLine pos1 bs1 : fls2@(FieldLine pos2 _ : _))
  -- if first line is on the same line with field name:
  -- the indentation level is either
  -- 1. the indentation of left most line in rest fields
  -- 2. the indentation of the first line
  -- whichever is leftmost
  | positionRow pos == positionRow pos1 =
      concat $
        fromUTF8BS bs1
          : mealy (mk mcol1) pos1 fls2
  -- otherwise, also indent the first line
  | otherwise =
      concat $
        replicate (positionCol pos1 - mcol2) ' '
          : fromUTF8BS bs1
          : mealy (mk mcol2) pos1 fls2
  where
    mcol1 = foldl' (\a b -> min a $ positionCol $ fieldLineAnn b) (min (positionCol pos1) (positionCol pos2)) fls2
    mcol2 = foldl' (\a b -> min a $ positionCol $ fieldLineAnn b) (positionCol pos1) fls2

    mk :: Int -> Position -> FieldLine Position -> (Position, String)
    mk col p (FieldLine q bs) =
      ( q
      , replicate newlines '\n'
          ++ replicate indent ' '
          ++ fromUTF8BS bs
      )
      where
        newlines = positionRow q - positionRow p
        indent = positionCol q - col

mealy :: (s -> a -> (s, b)) -> s -> [a] -> [b]
mealy f = go
  where
    go _ [] = []
    go s (x : xs) = let ~(s', y) = f s x in y : go s' xs

fieldLinesToStream :: [FieldLine ann] -> FieldLineStream
fieldLinesToStream [] = fieldLineStreamEnd
fieldLinesToStream [FieldLine _ bs] = FLSLast bs
fieldLinesToStream (FieldLine _ bs : fs) = FLSCons bs (fieldLinesToStream fs)

fieldLinesToBS :: [FieldLine ann] -> BS.ByteString
fieldLinesToBS [] = mempty
fieldLinesToBS [FieldLine _ bs] = bs -- don't leave trailing newline
fieldLinesToBS (FieldLine _ bs : fls) = bs <> "\n" <> fieldLinesToBS fls

getFieldLinesFirstAnn :: [FieldLine ann] -> Maybe ann
getFieldLinesFirstAnn = fmap getAnn . safeHead
  where
    getAnn (FieldLine ann _) = ann
