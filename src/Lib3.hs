{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: move `tokenizeYaml`, `Token` to an internal module
module Lib3(hint, gameStart, parseDocument, tokenizeYaml, Token, GameStart, Hint) where

import Parser(Token, parseTokens, tokenizeYaml)
import Types ( Document(..), FromDocument, fromDocument)
import Lib1 (State(..))

type ColNo = [Int]
type RowNo = [Int]
type HintNo = Int

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = do
    let tokens = tokenizeYaml str
    -- parseDocument consists of four seperate steps:
    -- - tokenization
    -- - scalar parsing
    -- -- - filtering - gets rid of obviously wrong (i.e. easy to detect) cases
    -- - actual parsing from tokens into Document 
    --     i.e. mostly forming collections (lists, mappings) from tokens
    doc <- parseTokens tokens
    return $ fst doc

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart ColNo RowNo HintNo
instance FromDocument GameStart where
    fromDocument  doc = do
        colNo <- fromDMap doc >>= findDMap "occupied_cols" >>= fromDList >>= mapM fromDInteger >>= checkLength
        rowNo <- fromDMap doc >>= findDMap "occupied_rows" >>= fromDList >>= mapM fromDInteger >>= checkLength
        hintNo <- fromDMap doc >>= findDMap "number_of_hints" >>= fromDInteger
        return $ GameStart colNo rowNo hintNo

checkLength :: [Int] -> Either String [Int]
checkLength list = if length list == 10 then Right list else Left "Number of rows or cols != 10"

findDMap :: String -> [(String, Document)] -> Either String Document
findDMap key ((dKey, doc):xs) =   if dKey ==  key then Right doc else findDMap key xs
findDMap _ _ = Left "Unable to find value with specified key"

fromDMap :: Document -> Either String [(String, Document)]
fromDMap (DMap m) = Right m
fromDMap _ = Left "Document is not a DMap"

fromDList :: Document -> Either String [Document]
fromDList (DList l) = Right l
fromDList _ = Left "Document is not a DList"

fromDInteger :: Document -> Either String Int
fromDInteger (DInteger i) = Right i
fromDInteger _ = Left "Document is not a DInteger"

fromDString :: Document -> Either String String
fromDString (DString i) = Right i
fromDString _ = Left "Document is not a DString"

fromDNull :: Document -> Either String (Maybe a)
fromDNull DNull = Right Nothing
fromDNull _ = Left "Document is not a DNull"

isDNull :: Document -> Bool
isDNull DNull = True
isDNull _ = False

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart _ (GameStart colNo rowNo hintNo) = State [] [] colNo rowNo hintNo

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint deriving Show
instance FromDocument Hint where
    fromDocument _ = error "NOT IMPLEMENTED"

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint s _ = s
-- hint (State l) h = State $ ("Hint " ++ show h) : l
