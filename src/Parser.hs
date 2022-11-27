{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Parser(Token(..), tokenizeYaml, parseTokens) where

import Types(Document(..), ToDocument, toDocument)
import qualified Control.Monad.Trans.Error
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Control.Applicative ((<|>))

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

--                         Indentation Token on the line
-- data InfoToken = InfoToken Int         [Token]

instance ToDocument Token where
    toDocument (TokenScalarString str) = DString str
    toDocument (TokenScalarInt int) = DInteger int
    toDocument TokenScalarNull = DNull
    -- toDocument _ = undefined

tokenizeYaml :: String -> [Token]
tokenizeYaml "" = []
tokenizeYaml str = (normalizeWhitespace . parseScalar . joinBetweenScalar . joinUnknown . tokenizeYaml') str
    where
        tokenizeYaml' :: String -> [Token]
        tokenizeYaml' "" = []
        tokenizeYaml' str' =  do
            let (tokens, left) = tokenize str'
            if left /= "" then tokens ++ tokenizeYaml' left else tokens

        joinUnknown :: [Token] -> [Token]
        joinUnknown [] = []
        joinUnknown tokens = do
            let (ok, begin) = break isTokenUnknown tokens
            let (unknownChars, left) = span isTokenUnknown begin
            if null unknownChars then ok ++ joinUnknown left
            else
                ok ++ TokenScalarUnknown (foldr func "" unknownChars ) : joinUnknown left
            where
                isTokenUnknown (TokenUnknown _) = True
                isTokenUnknown _ = False

                func :: Token -> String -> String
                func (TokenUnknown t') acc = t':acc
                func _ _ = undefined
                -- `unknownChars` is ensured to be a list of only TokenUnknown
                -- thus `getUnknown` is called only to unpack the char inside TokenUnknown

        normalizeWhitespace :: [Token] -> [Token]
        normalizeWhitespace tokens = snd $ normalizeWhitespace'' tokens
            where
                normalizeWhitespace'' :: [Token] -> ([Token], [Token])
                normalizeWhitespace'' [] = ([], [])
                normalizeWhitespace'' (t:ts)
                    | isTokenSpace t = normalizeWhitespace' (t:ts, [])
                    | otherwise = normalizeWhitespace' (t:ts, [TokenSpace 0])

                normalizeWhitespace' :: ([Token], [Token]) -> ([Token], [Token])
                normalizeWhitespace' ([], ts') = ([], ts')
                normalizeWhitespace' (TokenNewLine:tkn:ts, ts')
                    | isTokenSpace tkn = normalizeWhitespace' (tkn:ts, ts' ++ [TokenNewLine])
                    | otherwise = normalizeWhitespace' (tkn:ts,ts' ++ [TokenNewLine, TokenSpace 0])
                normalizeWhitespace' (tkn:ts, ts') = normalizeWhitespace' (ts, ts' ++ [tkn])

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
        tokenize string
            | "- " `isPrefixOf` string = ([TokenDashListItem], drop 2 string)
            | "-\n" `isPrefixOf` string = ([TokenDashListItem, TokenNewLine], drop 2 string)
            | "[" `isPrefixOf` string = ([TokenBeginBracketList], drop 1 string)
            | "]" `isPrefixOf` string = ([TokenEndBracketList], drop 1 string)
            | "{" `isPrefixOf` string = ([TokenBeginMapping], drop 1 string)
            | "}" `isPrefixOf` string = ([TokenEndMapping], drop 1 string)
            | "," `isPrefixOf` string = ([TokenCollectionSep], drop 1 string)
            | ": " `isPrefixOf` string = ([TokenKeyColon], drop 2 string)
            | ":\n" `isPrefixOf` string = ([TokenKeyColon, TokenNewLine], drop 2 string)
            | "\"" `isPrefixOf` string = ([TokenScalarString $ fst stringQ], snd stringQ)
            | "'" `isPrefixOf` string = ([TokenScalarString $ fst stringA], snd stringA)
            | " " `isPrefixOf` string = ([TokenSpace (length (fst spaced))], snd spaced)
            | "\n" `isPrefixOf` string = ([TokenNewLine], drop 1 string)
            | otherwise = getUnknown string
                where
                    -- functions for strings
                    spaced = span (== ' ') string
                    stringQ = do
                        let str' = getStr string '"' '\\'
                        -- TODO: figure out when unescaping '\*' is need and implement unescaping
                        str'
                    stringA = getStr string '\'' '\\'

                    getStr :: String -> Char -> Char -> (String, String)
                    getStr s char excl = do
                        let str'' = drop 1 s
                        let (striped, left) = span (/= char) str''
                        if last striped /= excl then
                            (striped, drop 1 left)
                        else
                            let (striped', left') = getStr left char excl
                                striped'' = striped ++ [char] ++ striped'
                            in (striped'', left')

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
                                unJust Nothing = undefined
                func t = t
            -- parsers :: [Token] -> Either String (Document, [Token])
            -- parsers = colParsers ++ scalarParsers
            -- colParsers = [parseListOfOneDash]; scalarParsers = [parseSingleScalar]

parseSingleScalar :: [Token] -> Either String (Document, [Token])
-- parseSingleScalar [] = Right (DNull, [])
parseSingleScalar (TokenScalarNull:ts) = Right (DNull, ts)
parseSingleScalar (TokenScalarInt i:ts) = Right (DInteger i, ts)
parseSingleScalar (TokenScalarString str:ts) = Right (DString str, ts)
parseSingleScalar _ = Left "Not a single scalar"

-- parseNewLine :: [Token] -> Either String (Document, [Token])
-- parseNewLine [TokenNewLine] = Right (DNull, []) 
            -- parseListOfDash' (docs, [TokenNewLine]) = do return (docs, [])
            -- parseListOfDash' (docs, []) = Right (docs, [])

parseListOfOneDash :: [Token] -> Either String (Document, [Token])
parseListOfOneDash tkns = do
    list <- parseListOfDash' ([], tkns)
    return (DList (fst list), snd list)
        where
            parseListOfDash' :: ([Document], [Token]) -> Either String ([Document], [Token])
            parseListOfDash' (list, tokens) = do
                (_, tokens') <- expectIndentation tokens
                (_, tokens'') <- expectToken TokenDashListItem tokens'
                (d, tokens''') <- parseTokens tokens''
                let nlE = expectToken TokenNewLine tokens'''
                case nlE of
                    Left _ -> return (list ++ [d], tokens'')
                    Right (_, tokens'''') -> parseListOfDash' (list ++ [d], tokens'''')


expectIndentation :: [Token] -> Either String (Int, [Token])
expectIndentation (TokenSpace s:ts) = Right (s, ts)
expectIndentation (t:_) = Left $ "Expected TokenSpace, but got: " ++ show t
expectIndentation [] = Left "Empty token list"

expectToken :: Token -> [Token] -> Either String (Token, [Token])
expectToken _ [] = Left "Empty token list"
expectToken tk (t:ts)
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
firstRight funcs failMsg el = foldr (\func acc -> acc <|> func el) (Left failMsg) funcs

parseTokens :: [Token] -> Either String (Document, [Token])
parseTokens tokens = parseSingleScalar tokens <|> parseListOfOneDash tokens
