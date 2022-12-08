{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Eta reduce" #-}
module Parser(parse, Token(..), tokenizeYaml) where

import Types(Document(..), ToDocument, toDocument)
-- import qualified Control.Monad.Trans.Error
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Control.Applicative

-- - Raktai neturės "specialių" simbolių, o tik raides
-- - Eilutės galimos tik raidės, skaitmenys ir tarpas
-- - Eilutės maksimalus ilgis 16
-- - Rakto maksimalus ilgis 10

newtype Parser a = Parser
  { runParser :: [Token] -> Either String (a, [Token])
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (x, input') <- p input
      return (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (f, input') <- p1 input
      (a, input'') <- p2 input'
      return (f a, input'')

instance Alternative Parser where
  empty = Parser $ const $ Left "ERR_00: No parser"
  (Parser p1) <|> (Parser p2) =
      Parser $ \input -> p1 input <|> p2 input

-- YamlToken is purposefully not recursive
-- `tokenizeYaml` makes a guarante what
--  tokens starting with `TokenInternal`
--  do not show up then tokenized
--  if encoutered after `tokenizeYaml`
--  consider it a bug or an oversight in `tokenizeYaml`
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
    deriving (Show, Eq)

toYamlStr :: Token -> String
toYamlStr (TokenScalarString str) = str
toYamlStr (TokenScalarInt int) = show int
toYamlStr TokenScalarNull = "null"
toYamlStr _ = undefined

isTokenNewLine TokenNewLine = True
isTokenNewLine _ = False

isTokenSpace (TokenSpace _) = True
isTokenSpace _              = False

isTokenScalar (TokenScalarInt _)    = True
isTokenScalar (TokenScalarString _) = True
isTokenScalar TokenScalarNull       = True
isTokenScalar _                     = False

isTokenSpaceDiff (TokenSpaceDiff _) = True
isTokenSpaceDiff _                  = False

removeTokenSpaceDiff :: [Token] -> [Token]
removeTokenSpaceDiff ((TokenSpaceDiff _):ts) = ts
removeTokenSpaceDiff ts = ts

instance ToDocument Token where
    toDocument (TokenScalarString str) = DString str
    toDocument (TokenScalarInt int) = DInteger int
    toDocument TokenScalarNull = DNull
    toDocument _ = undefined

-- -- -- tokenize -- -- --

tokenizeYaml :: String -> [Token]
tokenizeYaml "" = []
tokenizeYaml str = pipeline str
    where
        pipeline str' = (
            normalizeDashListItem .
            splitDiff .
            diff .
            normalizeWhitespace .
            parseScalarToken .
            joinBetweenScalar .
            joinUnknown .
            tokenizeYaml' .
            ensureNL
            ) (str' ++ "\n")

        -- Currently, this doesn't handle cases when there is a `TokenSpace` between `TokenDashListItem`s
        -- multiline json like lists and mappings are also unsupported
        -- 
        -- This changes lines like these:
        -- '- - value1'
        -- '  - value2'
        --   into
        -- '-         '
        -- '  - value1'
        -- '  - value2'
        normalizeDashListItem :: [Token] -> [Token]
        normalizeDashListItem tkns = snd $ normalizeDashListItem' (tkns, [])
            where
                normalizeDashListItem' :: ([Token], [Token]) -> ([Token], [Token])
                normalizeDashListItem' ([], ts') = ([], ts')
                normalizeDashListItem' (TokenDashListItem:TokenDashListItem:ts, ts' ) = do
                    let (bef, after) = break isTokenSpaceDiff ts

                    normalizeDashListItem' (
                            TokenDashListItem:(bef ++ removeTokenSpaceDiff after),
                            ts' ++ [TokenDashListItem, TokenNewLine, TokenSpaceDiff 2]
                        )
                normalizeDashListItem' (tkn:ts, ts') = normalizeDashListItem' (ts, ts' ++ [tkn])

        tokenizeYaml' :: String -> [Token]
        tokenizeYaml' "" = []
        tokenizeYaml' str' =  do
            let (tokens, left) = tokenize str'
            if left /= "" then tokens ++ tokenizeYaml' left else tokens

        ensureNL :: String -> String
        ensureNL str' =
            case last str' of
                '\n' -> str'
                _ -> str' ++ "\n"

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

        -- gets rid of leading `TokenSpace` and introduces `TokenSpaceDiff`
        diff :: [Token] -> [Token]
        diff ((TokenSpace s):tokens) = snd $ diff' (tokens, []) s
            where
                diff' :: ([Token], [Token]) -> Int -> ([Token], [Token])
                diff' ([], ts) _ = ([], ts)
                diff' (TokenNewLine:(TokenSpace s'):ts, ts') prev
                    | s' == prev = diff' (ts, ts' ++ [TokenNewLine]) s'
                    | otherwise = diff' (ts, ts' ++ [TokenNewLine, TokenSpaceDiff (s' - prev)]) s'
                diff' (tkn:ts, ts') prev = diff' (ts, ts' ++ [tkn]) prev
        diff (_:_) = undefined
        diff [] = []

        -- this should fix testCase "Complex list"
        -- |-         |
        -- |  -       | <-  TokenSpaceDiff 2
        -- |    - foo | <-  TokenSpaceDiff 2
        -- |- bar       <-  splits this 'clif' (TokenSpaceDiff (-4)) into several `TokenSpaceDiff`s ([TokenSpaceDiff (-2), TokenSpaceDiff (-2)])
        splitDiff :: [Token] -> [Token]
        splitDiff tkns = do
            let (emt, _, res) = splitDiff' tkns [] []

            if not (null emt) then error "emt is not empt"
            -- else if not ( null ind) then error "ind is not empty"
            else res

            where
                splitDiff' :: [Token] -> [Int] -> [Token] -> ([Token], [Int], [Token])
                splitDiff' [] acc ts = ([], acc, ts)
                splitDiff' ((TokenSpaceDiff ind):ts) acc ts'
                    -- decline, compare with previous diffs and split it if needed
                    | ind < 0 =
                        case acc of
                            [] -> undefined -- access without previous history should never happen
                            (a:acc') ->
                                -- assumptions: a > 0,  ind < 0
                                -- equal decrese and increase
                                if a + ind == 0 then splitDiff' ts acc' (ts' ++ [TokenSpaceDiff ind])
                                -- decrese and increase don't match
                                else splitDiff' (TokenSpaceDiff (a + ind):ts) acc' (ts' ++ [TokenSpaceDiff (a +ind)])
                    -- incline, save to acc
                    | ind > 0 = splitDiff' ts (ind:acc) (ts' ++ [TokenSpaceDiff ind])
                splitDiff' (t:ts) acc ts' = splitDiff' ts acc (ts' ++ [t])

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
            -- -- | "- - " `isPrefixOf` string = ([TokenDashListItem, TokenNewLine], drop 2 string)
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
                    getStr s char excl
                        | head (drop 1 s) == char = ("", drop 2 s)
                        | otherwise = do
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

-- -- -- parsing -- -- --

parse :: String -> Either String Document
parse str = do
    let tokens = tokenizeYaml str
    (doc, _) <- runParser parseYaml tokens
    return doc

parseYaml = parseIndented <|> parseMap <|> parseList <|> parseJsonLike
parseJsonLike = parseJsonMap <|> parseJsonList <|> parseScalar
parseScalar =  parseNull <|> parseNumber <|> parseString

tokenPFac :: (Token -> Token -> Bool) -> (Token -> Parser Token)
tokenPFac cmpFunc = tokenP'
    where
        tokenP' t = Parser func
            where
                func (t':ts)
                    | t `cmpFunc` t' = Right (t', ts)
                    | otherwise = Left $ "ERR_01: Coudn't match a token, expected: " ++ show t ++ ", got: " ++ show (t':ts)
                func _ = Left "ERR_02A: Empty token list"

tokenPFac' :: (Token -> Token -> Bool) -> (Token -> Parser Token)
tokenPFac' cmpFunc = tokenP'
    where
        tokenP' t = Parser func
            where
                func (t':ts)
                    | t `cmpFunc` t' = Right (t', ts)
                    | otherwise = Right (t', t':ts)
                func _ = Left "ERR_02B: Empty token list"

tokenP :: Token -> Parser Token
tokenP = tokenPFac dumbCmp

scalarTokenP :: Parser Token
scalarTokenP = tokenPFac (\_ t2 -> isTokenScalar t2) $ TokenScalarInt 0

tokenPOptional :: Token -> Parser Token
tokenPOptional = tokenPFac' dumbCmp

dumbCmp :: Token -> Token -> Bool
dumbCmp (TokenScalarInt _) (TokenScalarInt _) = True
dumbCmp (TokenScalarString _) (TokenScalarString _) = True
dumbCmp (TokenSpaceDiff _) (TokenSpaceDiff _) = True
dumbCmp t1 t2 = t1 == t2

parseJsonList =
    tokenP TokenBeginBracketList *>
    (
        DList <$>
            ((((:) <$> one)
                <*>
            many (sep *> one))
                <|>
            pure [])
    )
    <* tokenP TokenEndBracketList
        where
            sep :: Parser Token
            sep = wsnlOptional *> tokenP TokenCollectionSep <* wsOptional
            one :: Parser Document
            one = wsnlOptional *> parseJsonMap' <|> parseJsonLike <* wsnlOptional

parseJsonMap' =
    wsnlOptional *>
    (
        DMap <$> some parseKeyValueJson
    )
    <* wsnlOptional

parseJsonMap =
    tokenP TokenBeginMapping *>
    wsnlOptional *>
    (
        DMap <$>
            ((((:) <$> one)
                <*>
            many (sep *> one))
                <|>
            pure [])
    )
    <* wsnlOptional
    <* tokenP TokenEndMapping
    where
        sep :: Parser Token
        sep = wsnlOptional *> tokenP TokenCollectionSep <* wsOptional
        one :: Parser (String, Document)
        one = wsnlOptional *> parseKeyValueJson <* wsnlOptional

parseNull :: Parser Document
parseNull = const DNull <$> tokenP TokenScalarNull

parseNumber :: Parser Document
parseNumber = func <$> tokenP (TokenScalarInt 0)
    where
        func :: Token -> Document
        func (TokenScalarInt int) = DInteger int
        func _ = undefined -- <- should not happen

parseString :: Parser Document
parseString = func <$> tokenP (TokenScalarString "")
    where
        func :: Token -> Document
        func (TokenScalarString str) = DString str
        func _ = undefined -- <- should not happen

spanP :: (Token -> Bool) -> Parser [Token]
spanP f =
    Parser $ \input ->
        let (token, rest) = span f input
        in Right (token, rest)

-- new line skip
nlOptional :: Parser [Token]
nlOptional = spanP isTokenNewLine

-- whitespace skip
wsOptional :: Parser [Token]
wsOptional = spanP isTokenSpace

wsnlOptional :: Parser [Token]
wsnlOptional = spanP (\t -> isTokenSpace t || isTokenNewLine t)

sdOptional :: Parser Token
sdOptional = tokenPOptional (TokenSpaceDiff 0)

sdStrict :: Parser Token
sdStrict = tokenP (TokenSpaceDiff 0)

parseList :: Parser Document
parseList = DList <$> (
                some one
            <|> (((:) <$> first) <*> pure [])
        )
    where
        first :: Parser Document
        first =
            tokenP TokenDashListItem *> wsnlOptional *> sdStrict *>
            parseYaml
            <* wsnlOptional

        one :: Parser Document
        one =
            tokenP TokenDashListItem *> wsnlOptional *>
            parseYaml
            <* wsnlOptional

parseIndented :: Parser Document
parseIndented =
    nlOptional *> sdStrict *>
    parseYaml
    <* wsOptional <* nlOptional <* sdOptional

parseMap :: Parser Document
parseMap = DMap <$> (
            ((((:) <$> first) <*> some one) <|> some one)
        <|> (((:) <$> first) <*> many last')
        <|> some one
    )
    where
        first :: Parser (String, Document)
        first = parseKeyValue <* wsnlOptional <* sdOptional

        last' = parseKeyValue <* wsnlOptional <* sdStrict
        one :: Parser (String, Document)
        one = parseKeyValue

parseKeyValue :: Parser (String, Document)
parseKeyValue =
      (\key _ value -> (key, value)) <$> getStringified <*>
      ( tokenP TokenKeyColon <* wsnlOptional) <*>
      parseYaml <* wsnlOptional


parseKeyValueJson :: Parser (String, Document)
parseKeyValueJson =
      (\key _ value -> (key, value)) <$> getStringified <*>
      ( tokenP TokenKeyColon <* wsOptional) <*>
      parseJsonLike <* wsnlOptional

getStringified :: Parser String
getStringified = toYamlStr <$> scalarTokenP
