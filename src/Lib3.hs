{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- TODO: move `tokenizeYaml`, `Token` to an internal module
module Lib3(hint, gameStart, parseDocument, tokenizeYaml, Token(..), GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument, ToDocument (toDocument) )
import Lib1 (State(..))
import Data.List ( isPrefixOf )
import Text.Read (readEither, readMaybe)
import Data.Either
import Data.Maybe
import Control.Applicative ( Alternative((<|>)) )
import qualified Control.Monad.Trans.Error
import Data.Monoid (Ap)

type ColNo = [Int]
type RowNo = [Int]
type HintNo = Int

-- - Raktai neturės "specialių" simbolių, o tik raides
-- - Eilutės galimos tik raidės, skaitmenys ir tarpas
-- - Eilutės maksimalus ilgis 16
-- - Rakto maksimalus ilgis 10

-- YamlToken is purposefully not recursive
data Token =
      TokenUnknown Char
    | TokenSpace Int
    | TokenScalarUnknown String
    --  ^ Any scalar type yet to be parsed, including string
    | TokenScalarString String
    | TokenScalarInt Int
    | TokenScalarNull
    --  ^ Verified to be a string, int, null
    | TokenKeyColon
    | TokenBeginMapping
    | TokenEndMapping
    | TokenDashListItem
    | TokenBeginBracketList
    | TokenEndBracketList
    | TokenCollectionSep
    | TokenNewLine
    deriving (Show, Eq)

instance ToDocument Token where
    toDocument (TokenScalarString str) = DString str
    toDocument (TokenScalarInt int) = DInteger int
    toDocument TokenScalarNull = DNull

tokenizeYaml :: String -> [Token]
tokenizeYaml "" = []
tokenizeYaml str = (parseScalar . joinBetweenScalar . joinUnknown . tokenizeYaml') str
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
                ok ++ TokenScalarUnknown (foldr func "" unknownChars ) : joinUnknown left
            where
                isTokenUnknown :: Token -> Bool
                isTokenUnknown (TokenUnknown _) = True
                isTokenUnknown _ = False

                func :: Token -> String -> String
                func t' acc = getUnknown t':acc
                    where
                        getUnknown :: Token -> Char
                        getUnknown (TokenUnknown c) = c
                        -- `unknownChars` is ensured to be a list of only TokenUnknown
                        -- thus `getUnknown` is called only to unpack the char inside TokenUnknown
                        getUnknown tkn =
                            error $
                                "This exception should never be thrown. Consider this a bug. `getUnknown` was called on:<" ++ show tkn ++ ">"

        joinBetweenScalar :: [Token] -> [Token]
        joinBetweenScalar tokens = snd $ joinBetweenScalar' (tokens, [])
            where
                joinBetweenScalar' :: ([Token], [Token]) -> ([Token], [Token])
                joinBetweenScalar' ([], ts) = ([], ts)
                -- Join spaces
                joinBetweenScalar'
                    ((TokenScalarUnknown str1):(TokenSpace times):(TokenScalarUnknown str2):ts, ts')
                        = joinBetweenScalar' (TokenScalarUnknown (str1 ++ replicate times ' ' ++ str2) : ts, ts')
                joinBetweenScalar' (t:ts, ts') = joinBetweenScalar' (ts, ts' ++ [t])

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
            | "\"" `isPrefixOf` str = ([TokenScalarString $ fst stringQ], snd stringQ)
            | "'" `isPrefixOf` str = ([TokenScalarString $ fst stringA], snd stringA)
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
                        let str'' = drop 1 s
                        let (striped, left) = span (/= char) str''
                        if last striped /= excl then
                            (striped, drop 1 left)
                        else
                            let (striped', left') = getStr left char excl
                                striped'' = striped ++ [char] ++ striped'
                            in (striped'', drop 1 left')

                    -- functions for scalars
                    getUnknown :: String -> ([Token], String)
                    getUnknown (c:str') = ([TokenUnknown c], str')
                    getUnknown "" = ([], "")

        parseScalar :: [Token] -> [Token]
        parseScalar tkns = map func tkns
            where
                func :: Token -> Token
                func (TokenScalarUnknown strU)
                    | strU == "null" = TokenScalarNull
                    | strU == "~" = TokenScalarNull
                    | otherwise = do
                        let mInt = (readMaybe strU :: Maybe Int)
                        if isJust mInt then TokenScalarInt (unJust mInt) else TokenScalarString strU
                            where
                                unJust (Just i) = i
                                unJust Nothing = error "Should not happen"
                func t = t

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

    doc <- firstRight parsers "" tokens
    return $ fst doc
        where
            parsers :: [[Token] -> Either String (Document, [Token])]
            parsers = colParsers ++ scalarParsers
            colParsers = [parseListOfOneDash]; scalarParsers = [parseSingleScalar]

            parseSingleScalar [] = Right (DNull, [])
            parseSingleScalar (TokenScalarNull:ts) = Right (DNull, ts)
            parseSingleScalar (TokenScalarInt i:ts) = Right (DInteger i, ts)
            parseSingleScalar (TokenScalarString str:ts) = Right (DString str, ts)
            parseSingleScalar _ = Left "Not a single scalar"

            parseListOfOneDash tokens = do
                list <- parseListOfDash' ([], tokens)
                return (DList (fst list), snd list)
                    where
                        parseListOfDash' :: ([Document], [Token]) -> Either String ([Document], [Token])
                        parseListOfDash' (docs, []) = do return (docs, [])
                        parseListOfDash' (docs, [TokenNewLine]) = do return (docs, [])
                        parseListOfDash' (list, tokens) = do
                            (_, tokens') <- parseToken TokenDashListItem tokens
                            (d, tokens'') <- takeScalar tokens'
                            let nlE = parseToken TokenNewLine tokens''
                            case nlE of
                                Left _ -> return (list ++ [d], tokens'')
                                Right (_, tokens''') -> parseListOfDash' (list ++ [d], tokens''')


                            -- let dashE = parseToken TokenDashListItem tokens'''


                            -- return (list ++ [d], tokens'')


            -- parseEmpty = takeWhile (\t -> (t == (TokenSpace _)) or (t == (TokenNewLine)))

            -- filter :: [Token] -> Either String [Token]
            -- filter tokens = filter' (tokens, [])
            --     where
            --         filter' :: ([Token], [Token]) -> Either String [Token]
            --         filter' ([], ts) = Right ts
            --         filter' (TokenDashListItem:TokenKeyColon:_, _) = Left "Excepted a key, found ':'"
            --         filter' (TokenDashListItem:(TokenSpace _):TokenKeyColon:_, _) = Left "Excepted a key, found ':'"
            --         filter' (TokenTokenKeyColon:)
            --         filter' (t:ts, ts') = filter' (ts, ts' ++ [t])

parseToken :: Token -> [Token] -> Either String (Token, [Token])
parseToken _ [] = Left "Empty token list"
parseToken tk (t:ts)
    | tk == t = Right (t, ts)
    | otherwise = Left $ "Expected: " ++ show tk ++ ", but got: " ++ show t

takeScalar :: [Token] -> Either String (Document, [Token])
takeScalar (t:ts)
    | isTokenScalarNull   t = Right (toDocument t, ts)
    | isTokenScalarInt    t = Right (toDocument t, ts)
    | isTokenScalarString t = Right (toDocument t, ts)
    | isTokenSpace t = takeScalar ts
    | otherwise = Left $ show t ++ " is not a scalar, left: " ++ show ts
takeScalar [] = Left "EmptyTokenList"

-- takeCollection :: [Token] -> Either String (Document, [Token])

isTokenScalarInt (TokenScalarInt _) = True
isTokenScalarInt _ = False

isTokenScalarString (TokenScalarString _) = True
isTokenScalarString _ = False
-- getTokenScalarInt (TokenScalarString i) = Right i
-- getTokenScalarInt _ = Left "Not an TokenScalarInt"

isTokenScalarNull TokenScalarNull = True
isTokenScalarNull _ = False
-- getTokenScalarInt (TokenScalarString i) = Right i
-- getTokenScalarInt _ = Left "Not an TokenScalarInt"

isTokenSpace (TokenSpace _) = True
isTokenSpace _ = False

isTokenScalar (TokenScalarInt _)    = True
isTokenScalar (TokenScalarString _) = True
isTokenScalar TokenScalarNull       = True
isTokenScalar _ = False

firstRight :: Control.Monad.Trans.Error.Error l => [from -> Either l r] -> l -> from -> Either l r
-- firstRight :: (Foldable t1, Control.Monad.Trans.Error.Error a1) => t1 (t2 -> Either a1 a2) -> a1 -> t2 -> Either a1 a2
firstRight funcs failMsg el = foldr (\func acc -> acc <|> func el) (Left failMsg) funcs

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
