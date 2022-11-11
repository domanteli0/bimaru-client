{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

-- TODO: move `tokenizeDocument`, `Token` to an internal module
module Lib3(hint, gameStart, parseDocument, tokenizeDocument, Token(..), GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..))
import Data.List ( isPrefixOf )

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
data GameStart = GameStart deriving Show
instance FromDocument GameStart where
    fromDocument _ = error "NOT IMPLEMENTED"

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart s _ = s
-- gameStart (State l) d = State $ ("Game started: " ++ show d) : l

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
