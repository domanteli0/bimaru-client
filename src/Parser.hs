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
import GHC.Base (Alternative(some, empty))
import Data.Char (isSpace)
import Data.Either (isRight)

-- - Raktai neturės "specialių" simbolių, o tik raides
-- - Eilutės galimos tik raidės, skaitmenys ir tarpas
-- - Eilutės maksimalus ilgis 16
-- - Rakto maksimalus ilgis 10

-- YamlToken is purposefully not recursive
data Token =
      TokenInternalUnknown Char
    | TokenSpace Int
    | TokenSpaceDiff Int
    | TokenInternalScalarUnknown String
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
    | TokenEndOfDocument
    deriving (Show, Eq)

isTokenNewLine TokenNewLine = True
isTokenNewLine _ = False

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

--                         Indentation Token on the line
-- data InfoToken = InfoToken Int         [Token]

instance ToDocument Token where
    toDocument (TokenScalarString str) = DString str
    toDocument (TokenScalarInt int) = DInteger int
    toDocument TokenScalarNull = DNull
    -- toDocument _ = undefined

tokenizeYaml :: String -> [Token]
tokenizeYaml "" = []
tokenizeYaml str = ((diff . normalizeWhitespace . parseScalarToken . joinBetweenScalar . joinUnknown . tokenizeYaml') str) ++ [TokenEndOfDocument]
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
                ok ++ TokenInternalScalarUnknown (foldr func "" unknownChars ) : joinUnknown left
            where
                isTokenUnknown (TokenInternalUnknown _) = True
                isTokenUnknown _ = False

                func :: Token -> String -> String
                func (TokenInternalUnknown t') acc = t':acc
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

        diff :: [Token] -> [Token]
        diff ((TokenSpace s):tokens) = snd $ diff' (tokens, []) s
        -- normalizeWhitespace tokens = snd $ normalizeWhitespace' tokens
            where
                diff' :: ([Token], [Token]) -> Int -> ([Token], [Token])
                diff' ([], ts) _ = ([], ts)
                diff' (TokenNewLine:(TokenSpace s'):ts, ts') prev
                    | s' == prev = diff' (ts, ts' ++ [TokenNewLine]) s'
                    | otherwise = diff' (ts, ts' ++ [TokenNewLine, TokenSpaceDiff (s' - prev)]) s'
                diff' (tkn:ts, ts') prev = diff' (ts, ts' ++ [tkn]) prev
        diff (_:_) = undefined
        diff [] = []

        joinBetweenScalar :: [Token] -> [Token]
        joinBetweenScalar tokens = snd $ joinBetweenScalar' (tokens, [])
            where
                joinBetweenScalar' :: ([Token], [Token]) -> ([Token], [Token])
                joinBetweenScalar' ([], ts) = ([], ts)
                -- Join spaces
                joinBetweenScalar'
                    ((TokenInternalScalarUnknown str1):(TokenSpace times):(TokenInternalScalarUnknown str2):ts, ts')
                        = joinBetweenScalar' (TokenInternalScalarUnknown (str1 ++ replicate times ' ' ++ str2) : ts, ts')
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
                    getUnknown (c:str') = ([TokenInternalUnknown c], str')
                    getUnknown "" = ([], "")

        parseScalarToken :: [Token] -> [Token]
        parseScalarToken tkns = map func tkns
            where
                func :: Token -> Token
                func (TokenInternalScalarUnknown strU)
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

parseScalar :: [Token] -> Either String (Document, [Token])
-- parseSingleScalar [] = Right (DNull, [])
parseScalar (TokenScalarNull:ts) = Right (DNull, ts)
parseScalar (TokenScalarInt i:ts) = Right (DInteger i, ts)
parseScalar (TokenScalarString str:ts) = Right (DString str, ts)
parseScalar ts = Left $ "Not a scalar, rest of tokens: " ++ show ts

-- Document with whitespace only
-- parseEmpty :: [Token] -> Either String (Document, [Token])
-- parseEmpty ts = Right (Do)

-- parseSingleScalar :: [Token] -> Either String (Document, [Token])
-- parseSingleScalar [TokenSpace _, TokenScalarNull] = Right (DNull, [])
-- parseSingleScalar [TokenSpace _, TokenScalarNull, TokenNewLine] = Right (DNull, [])
-- parseSingleScalar [TokenSpace _, TokenScalarInt i] = Right (DInteger i, [])
-- parseSingleScalar [TokenSpace _, TokenScalarInt i, TokenNewLine] = Right (DInteger i, [])
-- parseSingleScalar [TokenSpace _, TokenScalarString str] = Right (DString str, [])
-- parseSingleScalar [TokenSpace _, TokenScalarString str, TokenNewLine] = Right (DString str, [])
-- parseSingleScalar _ = Left "Document of more than one scalar"

-- parseEndOfDocument :: [Token] -> Either String (Document, [Token])
-- parseEndOfDocument [TokenEndOfDocument] -> Right (, [Token])

-- parseNewLine :: [Token] -> Either String (Document, [Token])
-- parseNewLine [TokenNewLine] = Right (DNull, []) 
            -- parseListOfDash' (docs, [TokenNewLine]) = do return (docs, [])
            -- parseListOfDash' (docs, []) = Right (docs, [])

-- parseListOfOneDash :: [Token] -> Either String (Document, [Token])
-- parseListOfOneDash tkns = do
--     list <- parseListOfDash' ([], tkns)
--     return (DList (fst list), snd list)
--         where
--             parseListOfDash' :: ([Document], [Token]) -> Either String ([Document], [Token])
--             parseListOfDash' (doc, []) = Right (doc, [])
--             parseListOfDash' (list, tokens) = do
--                 (_ind, tokens') <- expectSpace tokens
--                 (_, tokens'') <- expectToken TokenDashListItem tokens'
--                 (d, tokens''') <- parseTokens tokens''
--                 (_, tokens'''') <- expectToken TokenNewLine tokens'''
--                 let nlE = expectToken TokenNewLine tokens''''
--                 case nlE of
--                     Left _ -> return (list ++ [d], tokens'')
--                     Right (_, tokens''''') -> parseListOfDash' (list ++ [d], tokens''''')


parseListOfDash :: [Token] -> Either String (Document, [Token])
parseListOfDash tokens = do
    (vals, tokens0) <- many' tokens regular
    -- (_, tokens1) <- ending tokens
    return (DList vals, tokens0)
        where
            regular :: [Token] -> Either String (Document, [Token])
            regular tokens' = do
                (_, tokens0') <- expectToken TokenDashListItem tokens'
                (val, tokens1') <- parseTokens tokens0' -- <|> parseIndented
                (_, tokens2') <- expectToken TokenNewLine tokens1'
                return (val, tokens2')

parseMapping :: [Token] -> Either String (Document, [Token])
parseMapping tokens = do
    (vals, tokens0) <- many' tokens regular
    -- (_, tokens1) <- ending tokens
    return (DMap vals, tokens0)
        where
            regular :: [Token] -> Either String ((String, Document), [Token])
            regular tokens' = do
                (val, tokens0') <- parseKeyValuePair' tokens'
                (_, tokens1') <- expectToken TokenNewLine tokens0'
                return (val, tokens1')

            -- ending :: [Token] -> Either String (Maybe Document, [Token])
            -- ending (TokenEndOfDocument:ts) = 
            --         TokenEndOfDocument -> 
            --         TokenSpaceDiff diff -> case diff of
            --             _ | diff > 0 = parseIndented

            -- one :: [Token] -> Either String (Document, [Token])
            -- one tokens' = do
            --     let tokens0' = wsnl tokens'
            --     (val, tokens1') <- parseTokens tokens0'
            --     let tokens2' = wsnl tokens1'
            --     return (val, tokens2')
            -- withComma :: [Token] -> Either String (Document, [Token])
            -- withComma tokens' = do
            --     let tokens0' = wsnl tokens'
            --     (val, tokens1') <- parseTokens tokens0'
            --     let tokens2' = wsnl tokens1'
            --     (_, tokens3') <- expectToken TokenCollectionSep tokens2'
            --     return (val, tokens3')

-- parseJSONLikeList :: [Token] -> Either String (Document, [Token])
-- parseJSONLikeList tokens = do
--     (_, tokens0) <- expectToken TokenBeginBracketList tokens
--     let tokens1 = wsnl tokens0
--     (values, tokens2) <- manyWithComma tokens1 <|> some' tokens1 one
--     let tokens3 = wsnl tokens2
--     (_, tokens4) <- expectToken TokenEndBracketList tokens3
--     return (DList values, tokens4)
--         where
--             manyWithComma :: [Token] -> Either String ([Document], [Token])
--             manyWithComma tokens' = do
--                 (values, tokens0') <- many' tokens' withComma
--                 let tokens1' = wsnl tokens0'
--                 (doc, tokens2') <- parseTokens tokens1'
--                 return (values ++ [doc], tokens2')

--             one :: [Token] -> Either String (Document, [Token])
--             one tokens' = do
--                 let tokens0' = wsnl tokens'
--                 (val, tokens1') <- parseTokens tokens0'
--                 let tokens2' = wsnl tokens1'
--                 return (val, tokens2')
--             withComma :: [Token] -> Either String (Document, [Token])
--             withComma tokens' = do
--                 let tokens0' = wsnl tokens'
--                 (val, tokens1') <- parseTokens tokens0'
--                 let tokens2' = wsnl tokens1'
--                 (_, tokens3') <- expectToken TokenCollectionSep tokens2'
--                 return (val, tokens3')

-- TODO: handle this
parseIndented :: [Token] ->  Either String (Maybe Document, [Token])
parseIndented tokens = do
    (_, tokens0) <- expectToken TokenNewLine tokens
    (ind, tokens1) <- expectDiff tokens0

    case ind of
         _  | ind > 0 -> do
                (doc, tokens2) <- parseTokens tokens1
                Right (Just doc, tokens2)
            | otherwise  ->  Right (Nothing, tokens1)

-- parseJSONLikeMap :: [Token] -> Either String (Document, [Token])
-- parseJSONLikeMap tokens = do
--     (_, tokens0) <- expectToken TokenBeginMapping tokens
--     let tokens1 = wsnl tokens0
--     (values, tokens2) <- parseJSONKeyValuePairs tokens1
--     let tokens3 = wsnl tokens2
--     (_, tokens4) <- expectToken TokenEndMapping tokens3
--     return (values, tokens4)

-- parseJSONKeyValuePairs :: [Token] -> Either String (Document, [Token])
-- parseJSONKeyValuePairs tokens = do
--     let tokens1 = wsnl tokens
--     (values, tokens2) <- manyWithComma tokens1 <|> some' tokens1 one
--     let tokens3 = wsnl tokens2
--     return (DMap values, tokens3)
--         where
--             manyWithComma :: [Token] -> Either String ([(String, Document)], [Token])
--             manyWithComma tokens' = do
--                 (values, tokens0') <- many' tokens' withComma
--                 let tokens1' = wsnl tokens0'
--                 (doc, tokens2') <- parseKeyValuePair' tokens1'
--                 return (values ++ [doc], tokens2')

--             one :: [Token] -> Either String ((String, Document), [Token])
--             one tokens' = do
--                 let tokens0' = wsnl tokens'
--                 (val, tokens1') <- parseKeyValuePair' tokens0'
--                 let tokens2' = wsnl tokens1'
--                 return (val, tokens2')
--             withComma :: [Token] -> Either String ((String, Document), [Token])
--             withComma tokens' = do
--                 let tokens0' = wsnl tokens'
--                 (keyval, tokens1') <- parseKeyValuePair' tokens0'
--                 let tokens2' = wsnl tokens1'
--                 (_, tokens3') <- expectToken TokenCollectionSep tokens2'
--                 return (keyval, tokens3')

parseKeyValuePair' :: [Token] -> Either String ((String, Document), [Token])
parseKeyValuePair' tokens = do
    (key, tokens1) <- expectString tokens
    let tokens2 = wsnl tokens1
    (_, tokens3) <- expectToken TokenKeyColon tokens2
    let tokens4 = wsnl tokens3
    (doc, tokens5) <- parseTokens tokens4
    return ((key, doc), tokens5)

-- whitespace or newline
wsnl :: [Token] -> [Token]
wsnl = dropWhile (\t -> isTokenSpace t || isTokenNewLine t)
-- sepBy ::
--     ([Token] -> Either String (Document, [Token])) ->
--     ([Token] -> Either String (Document, [Token])) ->
--     ([Token] -> Either String ([Document], [Token]))
-- sepBy sep el = (:) <$> el <*> many' (sep *> el) <|> (Right [])

-- ignoreSpacingEq 

-- expectTokens :: [Token] -> [Token] -> Either String [Token]


optionalToken :: Token -> [Token] -> Either String [Token]
optionalToken _ [] = Left "Empty token list"
optionalToken (TokenSpace _) ((TokenSpace _):ts) = Right ts
optionalToken tk (t:ts)
    | tk == t = Right ts
    | otherwise = Right (t:ts)

expectDiff :: [Token] -> Either String (Int, [Token])
expectDiff (TokenSpaceDiff s:ts) = Right (s, ts)
expectDiff ts = Left $ "Expected TokenSpaceDiff, but got: " ++ show ts

expectSpace :: [Token] -> Either String (Int, [Token])
expectSpace (TokenSpace s:ts) = Right (s, ts)
expectSpace ts = Left $ "Expected TokenSpace, but got: " ++ show ts
-- expectRegardlessOfValue (TokenScalarString s:ts) = Right (s, ts)

expectString :: [Token] -> Either String (String, [Token])
expectString (TokenScalarString s:ts) = Right (s, ts)
expectString ts = Left $ "Expected TokenString, but got: " ++ show ts

expectSpaceOrNL :: [Token] -> Either String [Token]
expectSpaceOrNL (TokenSpace _:ts) = Right ts
expectSpaceOrNL (TokenNewLine:ts) = Right ts
expectSpaceOrNL ts = Left $ "Expected TokenSpace, or TokenNL but got: " ++ show ts

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

firstRight :: Control.Monad.Trans.Error.Error l => [from -> Either l r] -> l -> from -> Either l r
firstRight funcs failMsg el = foldr (\func acc -> acc <|> func el) (Left failMsg) funcs

allRight :: Control.Monad.Trans.Error.Error l => [from -> Either l r] -> l -> from -> [Either l r]
allRight funcs failMsg el = foldr (\func acc -> if isRight (func el) then func el : acc else acc) [Left failMsg] funcs

-- parseJSONLikeMaping = undefined

-- parseJSONToken :: [Token] -> Either String (Document, [Token])
-- parseJSONToken tokens = parseJSONLikeMap tokens <|> parseJSONLikeList tokens <|> parseScalar tokens

-- parseTokens' :: [Token] -> Either String (Document, [Token])
-- parseTokens' tokens = parseJSONToken tokens <|> parseListOfOneDash tokens
-- parseTokens' tokens = parseJSONToken tokens <|> parseListOfOneDash tokens

-- parseTokens :: [Token] -> Either String (Document, [Token])
-- parseTokens tokens =  parseTokens' tokens <|>  parseSingleScalar tokens
-- parseTokens tokens = parseJSONLikeMap tokens

parsers = [parseScalar, parseMapping, parseListOfDash]
-- parsers = [parseJSONLikeMap, parseListOfOneDash, parseScalar, parseListOfOneDash]

parseTokens :: [Token] -> Either String (Document, [Token])
parseTokens tokens = firstRight parsers "nope" tokens
    -- let goodOnes = allRight parsers "nope" tokens
    -- if null goodOnes then Left "Nope" else Right $ head $ allRight parsers "nope" tokens
    -- where
    --     parseTokens' :: [Either String (Document, [Token])] -> [Token] -> [Either String (Document, [Token])]
    --     parseTokens' = do
    --         (doc, tkns) <- allRight parsers "Nope" tokens
    --         if null tkns then return [(doc, [])] else return $ parseTokens' parsers tokens

-- parseTokens tokens = parseJSONLikeMap tokens <|> parseJSONLikeList tokens
-- parseTokens tokens = parseJSONLikeMap tokens <|> parseJSONLikeList tokens <|> parseScalar tokens

some' :: [Token] -> ([Token] -> Either String (a, [Token])) -> Either String ([a], [Token])
some' tkns parser = some'' tkns []
    where
        some'' s [] =
             case parser s of
                Left _ -> Right ([], s)
                Right (i, r) -> some'' r [i]
        some'' s acc =
            case parser s of
                Left _ -> Right (reverse acc, s)
                Right (i, r) -> some'' r (i:acc)

many' :: [t] -> ([t] -> Either s (d, [t])) -> Either s ([d], [t])
many' tkns parser = many'' tkns []
    where
        many'' s [] =
             case parser s of
                Left e -> Left e
                Right (i, r) -> many'' r [i]
        many'' s acc =
            case parser s of
                Left _ -> Right (reverse acc, s)
                Right (i, r) -> many'' r (i:acc)