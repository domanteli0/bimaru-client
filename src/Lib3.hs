{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use null" #-}

-- TODO: move `tokenizeYaml`, `Token` to an internal module
module Lib3(hint, gameStart, parseDocument, tokenizeYaml, Token(..), GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..))
import Data.List ( isPrefixOf )

type ColNo = [Int]
type RowNo = [Int]
type HintNo = Int

-- YamlToken is purposefully not recursive
data Token =
      TokenUnknown Char
    | TokenSpace Int
    | TokenScalar String
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

tokenizeYaml :: String -> [Token]
tokenizeYaml "" = []
-- `scalarize` - some pairs of tokens will have to be turned into TokenScalar
-- for example: [TokenScalar "a", TokenDashListItem, TokenScalar "b"]
-- tokenizeYaml str = scalarize $ joinUnknown $ tokenizeYaml' str
tokenizeYaml str = joinUnknown $ tokenizeYaml' str
    where
        tokenizeYaml' :: String -> [Token]
        tokenizeYaml' "" = []
        tokenizeYaml' str' =  do
            let (tokens, left) = tokenize str'
            if left /= "" then
                tokens ++ tokenizeYaml' left
            else
                tokens

        joinUnknown :: [Token] -> [Token]
        joinUnknown [] = []
        joinUnknown tokens = do
            let (ok, begin) = break isTokenUnknown tokens
            let (unknownChars, left) = span isTokenUnknown begin
            if unknownChars == [] then ok ++ joinUnknown left
            else 
                ok ++ TokenScalar (foldr func "" unknownChars ) : joinUnknown left

        func :: Token -> String -> String
        func t' acc = getUnknown t':acc
            where
                getUnknown :: Token -> Char
                getUnknown (TokenUnknown c) = c
                -- `unknownChars` is ensured to be a list of only TokenUnknown
                -- thus `getUnknown` is called only to unpack the char inside TokenUnknown
                getUnknown tkn =
                    error $
                        "This exception should never be thrown. Consider this a bug. `getUnknown` was called on:<" ++ show tkn ++ "> in"

isTokenUnknown :: Token -> Bool
isTokenUnknown (TokenUnknown _) = True
isTokenUnknown _ = False

tokenize :: String -> ([Token], String)
tokenize str
    | "- " `isPrefixOf` str = ([TokenDashListItem], drop 2 str)
    | "-\n" `isPrefixOf` str = ([TokenDashListItem, TokenNewLine], drop 2 str)
    | "[" `isPrefixOf` str = ([TokenBeginBracketList], drop 1 str)
    | "]" `isPrefixOf` str = ([TokenEndBracketList], drop 1 str)
    | "{" `isPrefixOf` str = ([TokenBeginMapping], drop 1 str)
    | "}" `isPrefixOf` str = ([TokenEndMapping], drop 1 str)
    | "," `isPrefixOf` str = ([TokenCollectionSep], drop 1 str)
    | ": " `isPrefixOf` str = ([TokenKeyColon], drop 2 str)
    | ":\n" `isPrefixOf` str = ([TokenKeyColon, TokenNewLine], drop 2 str)
    | "\"" `isPrefixOf` str = ([TokenString $ fst stringQ], snd stringQ)
    | "'" `isPrefixOf` str = ([TokenString $ fst stringA], snd stringA)
    | " " `isPrefixOf` str = ([TokenSpace (length (fst spaced))], snd spaced)
    | "\n" `isPrefixOf` str = ([TokenNewLine], drop 1 str)
    | otherwise = getUnknown str
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
            getUnknown :: String -> ([Token], String)
            getUnknown (c:str') = ([TokenUnknown c], str')
            getUnknown "" = error "This should not happen"

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = do 
    let tokens = tokenizeYaml str

    Left "NOT IMPLEMENTED"
        where
            parsers = []

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
comb :: Either String a -> (a -> Either String b) -> Either String b
comb (Left msg) _ = Left msg
comb (Right q) foo = foo q

data GameStart = GameStart ColNo RowNo HintNo
instance FromDocument GameStart where
    fromDocument  doc = do
        colNo <- fromDMap doc `comb` findDMap "occupied_cols" `comb` fromDList `comb` mapM fromDInteger `comb` checkLength
        rowNo <- fromDMap doc `comb` findDMap "occupied_rows" `comb` fromDList `comb` mapM fromDInteger `comb` checkLength
        hintNo <- fromDMap doc `comb` findDMap "number_of_hints" `comb` fromDInteger
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
