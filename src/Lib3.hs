{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

-- TODO: move `tokenizeDocument`, `Token` to an internal module
module Lib3(hint, gameStart, parseDocument, tokenizeDocument, Token(..), GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..))
import Data.List ( isPrefixOf )

type ColNo = [Int]
type RowNo = [Int]
type HintNo = Int

-- YamlToken is purposefully not recursive
data Token =
      TokenNull
    | TokenSpace Int
    | TokenMaybeScalar String
    -- ^ Any scalar type yet to be parsed
    -- NOTE: this still might be a string
    -- even though `TokenString` exists
    --
    -- NOTE: this might not be scalar at all
    -- just something that doesn't make sence
    | TokenString String
    -- ^ This is verified to be a string
    | TokenKeyColon
    | TokenBeginMapping
    | TokenEndMapping
    | TokenDashListItem
    | TokenBeginBracketList
    | TokenEndBracketList
    | TokenCollectionSep
    | TokenNewLine
    deriving (Show, Eq)

tokenizeDocument :: String -> [Token]
tokenizeDocument "" = []
tokenizeDocument str = do
    let (token, left) = tokenize str
    if left /= "" then
        token : tokenizeDocument left
    else
        [token]

tokenize :: String -> (Token, String)
tokenize str
    | "null" `isPrefixOf` str = (TokenNull, drop 4 str)
    | "~" `isPrefixOf` str = (TokenNull, drop 1 str)
    | "- " `isPrefixOf` str = (TokenDashListItem, drop 2 str)
    | "[" `isPrefixOf` str = (TokenBeginBracketList, drop 1 str)
    | "]" `isPrefixOf` str = (TokenEndBracketList, drop 1 str)
    | "{" `isPrefixOf` str = (TokenBeginMapping, drop 1 str)
    | "}" `isPrefixOf` str = (TokenEndMapping, drop 1 str)
    | "," `isPrefixOf` str = (TokenCollectionSep, drop 1 str)
    | ":" `isPrefixOf` str = (TokenKeyColon, drop 1 str)
    | "\"" `isPrefixOf` str = (TokenString $ fst stringQ, snd stringQ)
    | "'" `isPrefixOf` str = (TokenString $ fst stringA, snd stringA)
    | " " `isPrefixOf` str = (TokenSpace (length (fst spaced)), snd spaced)
    | "\n" `isPrefixOf` str = (TokenNewLine, drop 1 str)
    | otherwise = getScalar str
        where
            -- functions for strings
            spaced = span (== ' ') str
            stringQ = do
                let str' = getStr str '"' '\\'
                -- TODO: figure out when unescaping '\*' is need and implement unescaping
                str'
            stringA = getStr str '\'' '\\'

            getStr :: String -> Char -> Char -> (String, String)
            getStr s char excl = do
                let str' = drop 1 s
                let (striped, left) = span (/= char) str'
                if last striped /= excl then
                    (striped, drop 1 left)
                else
                    let (striped', left') = getStr left char excl
                        striped'' = striped ++ [char] ++ striped'
                    in (striped'', drop 1 left')
            
            -- functions for scalars
            -- TODO: tokenize by char till `tokenize` pattern matches with other branch
            -- currently this is wrong behaviour
            getScalar :: String -> (Token, String)
            getScalar str' = do
                let (retSca, retStr) = span (=='\n') str'
                (TokenMaybeScalar retSca, retStr)

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = Left "NOT IMPLEMENTED"

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart ColNo RowNo HintNo
instance FromDocument GameStart where
    fromDocument  doc = do
        colNo <- case fromDMap doc of
            Left msg1 -> Left msg1
            Right q1 -> case findDMap "occupied_cols" q1 of
                Left msg2 -> Left msg2
                Right q2 -> case fromDList q2 of
                    Left msg3 -> Left msg3
                    Right q3 -> case mapM fromDInteger q3 of
                        Left msg4 -> Left msg4
                        Right q4 -> case checkLength q4 of
                            Left msg5 -> Left msg5
                            Right q5 -> Right q5

        rowNo <- case fromDMap doc of
            Left msg1 -> Left msg1
            Right q1 -> case findDMap "occupied_rows" q1 of
                Left msg2 -> Left msg2
                Right q2 -> case fromDList q2 of
                    Left msg3 -> Left msg3
                    Right q3 -> case mapM fromDInteger q3 of
                        Left msg4 -> Left msg4
                        Right q4 -> case checkLength q4 of
                            Left msg5 -> Left msg5
                            Right q5 -> Right q5

        hintNo <- case fromDMap doc of
            Left msg1 -> Left msg1
            Right q1 -> case findDMap "number_of_hints" q1 of
                Left msg2 -> Left msg2
                Right q2 -> case fromDInteger q2 of
                    Left msg3 -> Left msg3
                    Right q3 -> Right q3

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
