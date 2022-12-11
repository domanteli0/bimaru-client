{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use !!" #-}
-- module Parser(parse, Token(..), tokenizeYaml, first, last', one) where
module Parser(parse, Token(..), tokenizeYaml) where

-- TODO: remove Testing
import Testing -- <- for use in ghci
import Types(Document(..), ToDocument, toDocument)
-- import qualified Control.Monad.Trans.Error
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Control.Applicative
import Data.Text.Internal.Read (IParser(runP))
-- import Data.List (singleton)
-- import Control.Monad

-- - Raktai neturės "specialių" simbolių, o tik raides
-- - Eilutės galimos tik raidės, skaitmenys ir tarpas
-- - Eilutės maksimalus ilgis 16
-- - Rakto maksimalus ilgis 10

todo = error "TODO!"

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

comb :: Parser [a] -> Parser [a] -> Parser [a]
comb (Parser p1) (Parser p2) =
    Parser $ \input -> do
        (a, input') <- p1 input
        (a', input'') <- p2 input'

        return (a ++ a', input'')

combLOne :: Parser a -> Parser [a] -> Parser [a]
combLOne (Parser p1) (Parser p2) =
    Parser $ \input -> do
        (a, input') <- p1 input
        (a', input'') <- p2 input'

        return (a : a', input'')

-- combROne :: Parser [a] -> Parser a -> Parser [a]
combROne :: Parser a -> Parser a -> Parser [a]
combROne p1 (Parser p2) =
    Parser $ \input -> do
        (a, input') <- runParser (many p1) input
        (a', input'') <- p2 input'

        return (a ++ [a'], input'')

combOne :: Parser a -> Parser a -> Parser [a]
combOne (Parser p1) (Parser p2) =
    Parser $ \input -> do
        (a, input') <- p1 input
        (a', input'') <- p2 input'

        return ([a, a'], input'')

overrideLeft :: Parser a -> String -> Parser a
overrideLeft (Parser p1) str =
    Parser $ \input -> do
        case p1 input of
            Right r -> Right r
            Left _ -> Left str


-- YamlToken is purposefully not recursive
-- `tokenizeYaml` makes a guarante what
--  tokens starting with `TokenInternal`
--  do not show up then tokenized
--  if encoutered after `tokenizeYaml`
-- consider it a bug or an oversight in `tokenizeYaml`
data Token =
      TokenInternalUnknown Char
    | TokenSpace Int
    -- | TokenSpaceDiff Int
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
    | TokenNone
    -- ^ meant to symbolize what nothing was parsed, but shouldn't be an error
    deriving (Show, Eq)

toYamlStr :: Token -> String
toYamlStr (TokenScalarString str) = str
toYamlStr (TokenScalarInt int) = show int
toYamlStr TokenScalarNull = "null"
toYamlStr _ = undefined

isTokenNewLine TokenNewLine = True
isTokenNewLine _ = False

isTokenScalarInt (TokenScalarInt _) = True
isTokenScalarInt _ = False

isTokenScalarString (TokenScalarString _) = True
isTokenScalarString _ = False

isTokenScalarNull TokenScalarNull = True
isTokenScalarNull _ = False

isTokenSpace (TokenSpace _) = True
isTokenSpace _              = False

isTokenScalar (TokenScalarInt _)    = True
isTokenScalar (TokenScalarString _) = True
isTokenScalar TokenScalarNull       = True
isTokenScalar _                     = False

-- isTokenSpaceDiff (TokenSpaceDiff _) = True
-- isTokenSpaceDiff _                  = False

isTokenKeyColon TokenKeyColon  = True
isTokenKeyColon _              = False

isTokenDashListItem TokenDashListItem = True
isTokenDashListItem _ = False

-- removeTokenSpaceDiff :: [Token] -> [Token]
-- removeTokenSpaceDiff ((TokenSpaceDiff _):ts) = ts
-- removeTokenSpaceDiff ts = ts

removeTokenDashListItem :: [Token] -> [Token]
removeTokenDashListItem (TokenDashListItem:ts) = ts
removeTokenDashListItem ts = ts
-- isTokenEndOfDocument TokenEndOfDocument = True
-- isTokenEndOfDocument _ = False
--                         Indentation Token on the line
-- data InfoToken = InfoToken Int         [Token]

instance ToDocument Token where
    toDocument (TokenScalarString str) = DString str
    toDocument (TokenScalarInt int) = DInteger int
    toDocument TokenScalarNull = DNull
    toDocument _ = undefined

-- -- -- war crimes -- -- --

tokenizeYaml :: String -> [Token]
tokenizeYaml "" = []
tokenizeYaml str = pipeline str
    where
        pipeline str' = (
            -- normalizeDashListItem .
            -- splitDiff .
            -- diff .
            normalizeDashListItem .
            filter' .
            normalizeWhitespace .
            parseScalarToken .
            joinBetweenScalar .
            joinUnknown .
            tokenizeYaml' .
            ensureNL
            ) str'

        filter' :: [Token] -> [Token]
        filter' tkns = snd $ filter'' tkns []
            where
                filter'' :: [Token] -> [Token] -> ([Token], [Token])
                filter'' [] ts = ([], ts)
                filter'' (TokenDashListItem:(TokenSpace _):TokenNewLine:ts) ts' = filter'' ts (ts' ++ [TokenDashListItem, TokenNewLine])
                filter'' (t:ts) ts' = filter'' ts (ts' ++ [t])


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


        -- matches cases with nested lists on the same line, such as:
        -- '- - 5'
        -- '  - 6'
        -- '- 7  '
        normalizeDashListItem :: [Token] -> [Token]
        normalizeDashListItem tkns = snd $ normalizeDashListItem' tkns [] 0
            where
                normalizeDashListItem' :: [Token] -> [Token] -> Int -> ([Token], [Token])
                normalizeDashListItem' [] ts _ = ([], ts)

                normalizeDashListItem' (TokenDashListItem:TokenDashListItem:ts) ts' acc =
                    normalizeDashListItem' (TokenDashListItem:ts) (ts' ++ [TokenDashListItem, TokenNewLine, TokenSpace (acc + 2)]) (acc + 2)
                normalizeDashListItem' ((TokenSpace i):ts) ts' _ = normalizeDashListItem' ts (ts' ++ [TokenSpace i]) i

                normalizeDashListItem' (t1:ts) ts' acc
                    | isTokenDashListItem t1 && not (isTokenNewLine (head ts)) = do
                        let (line, rest) = break isTokenNewLine ts
                        normalizeDashListItem' rest (ts' ++ [TokenDashListItem, TokenNewLine, TokenSpace (acc + 2)] ++ line) (acc + 2)

                    | otherwise = normalizeDashListItem' ts (ts' ++ [t1]) acc

        -- adds `TokenSpace 0` at the start of line, where there are none, i think...
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

tokenPFac :: (Token -> Token -> Bool) -> (Token -> Parser Token)
tokenPFac cmpFunc = tokenP'
    where
        tokenP' t = Parser func
            where
                func (t':ts)
                    | t `cmpFunc` t' = Right (t', ts)
                    | otherwise = Left $ "ERR_01: Coudn't match a token, expected: " ++ show t ++ ", got: " ++ show (t':ts)
                func _ = Left "ERR_02A: Empty token list"

tokenPOpFac :: (Token -> Token -> Bool) -> (Token -> Parser Token)
tokenPOpFac cmpFunc = tokenP'
    where
        tokenP' t = Parser func
            where
                func (t':ts)
                    | t `cmpFunc` t' = Right (t', ts)
                    | otherwise = Right (t', t':ts)
                func [] = Right (TokenNone, [])
                -- func _ = Left "ERR_02B: Empty token list"

tokenP :: Token -> Parser Token
tokenP = tokenPFac dumbCmp

scalarTokenP :: Parser Token
scalarTokenP = tokenPFac (\_ t2 -> isTokenScalar t2) $ TokenScalarInt 1

tokenPOptional :: Token -> Parser Token
tokenPOptional = tokenPOpFac dumbCmp

tokenListP :: [Token] -> Parser [Token]
tokenListP = traverse tokenP

dumbCmp :: Token -> Token -> Bool
dumbCmp (TokenScalarInt _) (TokenScalarInt _) = True
dumbCmp (TokenScalarString _) (TokenScalarString _) = True
dumbCmp (TokenSpace _) (TokenSpace _) = True
dumbCmp t1 t2 = t1 == t2

singleton :: a -> [a]
singleton a = [a]

parseJsonList =
    wsOptional *> tokenP TokenBeginBracketList *>
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
            -- sep :: Parser Document
            sep :: Parser Token
            sep = wsOptional *> tokenP TokenCollectionSep <* wsOptional
            one :: Parser Document
            one = wsOptional *> parseJsonLike <* wsOptional

-- parseJsonMap = DMap <$> (some parseKeyValueJson <|> pure [])
parseJsonMap' =
    wsOptional *>
    (
        DMap <$> some parseKeyValueJson
    )
    <* wsOptional

parseJsonMap =
    wsOptional *> tokenP TokenBeginMapping *>
    wsOptional *>
    (
        DMap <$>
            ((((:) <$> one)
                <*>
            many (sep *> one))
                <|>
            pure [])
    )
    <* wsOptional
    <* tokenP TokenEndMapping
    where
        sep :: Parser Token
        sep = wsOptional *> tokenP TokenCollectionSep <* wsOptional
        one :: Parser (String, Document)
        one = wsOptional *> parseKeyValueJson <* wsOptional

parseNull :: Parser Document
parseNull = wsOptional *> (DNull <$ tokenP TokenScalarNull)

parseNumber :: Parser Document
parseNumber = wsOptional *> (func <$> tokenP (TokenScalarInt 0))
    where
        func :: Token -> Document
        func (TokenScalarInt int) = DInteger int
        func _ = undefined -- <- should not happen

parseString :: Parser Document
parseString = wsOptional *> (func <$> tokenP (TokenScalarString ""))
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

-- wsnlOptional :: Parser [Token]
-- wsnlOptional = spanP (\t -> isTokenSpace t || isTokenNewLine t)

-- wsnlsdOptional :: Parser [Token]
-- wsnlsdOptional = (spanP (\t -> isTokenSpace t || isTokenNewLine t)) *> tokenPOptional TokenSpaceDiff 0

-- sdOptional :: Parser Token
-- sdOptional = tokenPOptional (TokenSpaceDiff 0)

-- sdStrict :: Parser Token
-- -- sdStrict = overrideLeft (tokenP (TokenSpaceDiff 0)) "TokenSpaceDiff expected, but ERR_02A or ERR_01 happened"
-- sdStrict = tokenP (TokenSpaceDiff 0)

nlStrict :: Parser Token
nlStrict = tokenP TokenNewLine

unsafeGetTokenSpace :: Token -> Int
unsafeGetTokenSpace (TokenSpace i) = i
unsafeGetTokenSpace _ = undefined

parseTokenSpace :: [Token] -> Either String (Token, [Token])
parseTokenSpace ts = expectToken (TokenSpace 10) ts

expectToken :: Token -> [Token] -> Either String (Token, [Token])
expectToken ch [] = Left $ "Empty input: '" ++ show ch ++ " expected"
expectToken t (t':ts) | t `dumbCmp` t' = Right (t', ts)
                      | otherwise = Left $ show t ++ " expected, got: " ++ show (t':ts)

parseList :: Parser Document
parseList = Parser $ \input -> do
    (docs, ts) <- runParser parseList' input
    return (DList docs, ts)

parseList' :: Parser [Document]
parseList' = Parser $ \input -> do
    ((ind, doc), ts) <- runParser (first <* nlOptional) input
    (docs, ts') <- runParser (many (latter ind <* nlOptional)) ts

    Right (doc:docs, ts')

    where
        first :: Parser (Int, Document)
        first = Parser $ \input -> do
            (ind, ts) <- parseTokenSpace input
            let ind' = unsafeGetTokenSpace ind


            (_, ts') <- runParser (tokenP TokenDashListItem) ts
            case runParser (tokenP TokenDashListItem) ts' of
                Left _ -> do
                    (_, ts'') <- runParser (wsOptional <*  nlOptional) ts'
                    (doc, ts''') <- runParser parseYaml ts''
                    return ((ind', doc), ts''')
                Right (_, _) -> Left "same line is unsuported"
                    -- (_, ts''') <- runParser (wsOptional <*  nlOptional) ts''
                    -- (doc, ts'''') <- runParser (latter (ind' + 2)) ts'''
                    -- return ((ind' + 2, doc), ts'''')

        -- one :: Parser Document
        -- one = wsOptional *> tokenP TokenDashListItem *> parseScalar <* wsnlOptional
        latter :: Int -> Parser Document
        latter ind = Parser $ \input -> do
            (ind', ts) <- parseTokenSpace input
            let ind'' = unsafeGetTokenSpace ind'
            (_, ts') <- runParser ((tokenP TokenDashListItem) <* wsOptional <* nlOptional) ts
            (doc, ts'') <- runParser parseYaml ts'
            -- (ind, ) <- runParser tokenP TokenDashListItem

            if ind /= ind'' then
                Left "Mismacthed indentation" else
                -- error $ "input: " ++ show input
                Right (doc, ts'')

parseMap :: Parser Document
parseMap = Parser $ \input -> do
    (docs, ts) <- runParser parseMap' input
    return (DMap docs, ts)

parseMap' :: Parser [(String, Document)]
parseMap' = Parser $ \input -> do
    ((ind, doc), ts) <- runParser (first <* nlOptional) input
    (docs, ts') <- runParser (many (latter ind <* nlOptional)) ts

    Right (doc:docs, ts')

    where
        first :: Parser (Int, (String, Document))
        first = Parser $ \input -> do
            (ind, ts) <- parseTokenSpace input
            let ind' = unsafeGetTokenSpace ind
            (doc, ts') <- runParser parseKeyValue ts
            return ((ind', doc), ts')

        -- one :: Parser Document
        -- one = wsOptional *> tokenP TokenDashListItem *> parseScalar <* wsnlOptional
        latter :: Int -> Parser (String, Document)
        latter ind = Parser $ \input -> do
            (ind', ts) <- parseTokenSpace input
            let ind'' = unsafeGetTokenSpace ind'
            (doc, ts') <- runParser parseKeyValue ts
            -- (ind, ) <- runParser tokenP TokenDashListItem

            if ind /= ind'' then
                Left "Mismacthed indentation" else
                -- error $ "input: " ++ show input
                Right (doc, ts')

-- parseList = DList <$> some one''
--     where
--         one'' :: Parser Document
--         one'' =
--             tokenP TokenDashListItem *> wsnlOptional *>
--             parseYaml
--             <* wsnlOptional

parseE :: Parser a
parseE = Parser $ \input ->
    error $ show input

-- parseIndented :: Parser Document
-- parseIndented =
--     wsnlOptional *> sdStrict *>
--     parseYaml
--     <* wsnlOptional
--     -- <* sdStrict
--     <* sdStrict
    -- <* overrideLeft
    --     sdStrict
    --     "In `parseIndented` `TokenSpaceDiff` was expected, got ERR_02A or ERR_01 instead"


-- NOTE: `<*` has higher priority than `<|>`
-- parseMap :: Parser Document
-- -- parseMap = DMap <$> ((((:) <$> first) <*> many one) <|> many last')
-- parseMap = DMap <$> (

--         -- 👇 problematic misparses testCase "List (nl between `-` and map) with a nested map"
--         -- ((((:) <$> one') <*> many (last' <|> one' <|> one))) <|>

--         -- ((((:) <$> one') <*> many (one))) <|>
--         -- (((:) <$> first') <*> (many (last'' <|> one))) <|>
--         -- (((:) <$> first') <*> (many (last'' <|> one))) <|>

--         -- (((:) <$> first') <*> (many (last'' <|> one)))


--         -- map on list branch, i.e.: [DOESN'T WORK]  
--         -- normal branch, i.e.:
--         -- ' - key: stuuf '
--         -- '   keyy:      '
--         -- '     stuff    '
--         -- '   key: asd   '
--         (((:) <$> kvp <* sdStrict) <*> ((some kvp) <* sdStrict)) <|>
--         -- (((:) <$> first') <*> (many (last'' <|> one))) <|>
--         -- ((((:) <$> one) <*> many (one))) <|>

--         -- normal branch, i.e.:
--         -- ' -            '
--         -- '   key: stuuf '
--         -- '   keyy:      '
--         -- '       stuff  '
--         -- '   key: asd   '
--         some kvp

--         -- (((:) <$> one) <*> many (last' <|> one)) <|>
--     )
-- kvp = parseKeyValue
-- kvp = parseKeyValue <* wsOptional <* nlStrict -- <- doesn't work because other parsers might consume nl

-- parseKeyValueOnNL :: Parser (String, Document)
-- parseKeyValueOnNL =
--       (\key _ value -> (key, value)) <$> getStringified <*>
--       ( tokenP TokenKeyColon <* wsOptional <* nlOptional) <*>
--     --   parseYaml
--     --   parseYaml <* sdOptional
--       parseYaml <* sdStrict

parseKeyValue :: Parser (String, Document)
parseKeyValue =
      (\key _ value -> (key, value)) <$> getStringified <*>
      ( tokenP TokenKeyColon <* wsOptional <* nlOptional) <*>
      parseYaml


parseKeyValueJson :: Parser (String, Document)
parseKeyValueJson =
      (\key _ value -> (key, value)) <$> getStringified <*>
      ( tokenP TokenKeyColon <* wsOptional) <*>
      parseJsonLike <* wsOptional

parseKeyValueJsonWrapper :: Parser Document
parseKeyValueJsonWrapper = Parser $ \input -> do
    (kvp, rest) <- runParser parseKeyValueJson input
    return (DMap [kvp], rest)

getStringified :: Parser String
getStringified = toYamlStr <$> scalarTokenP

-- -- -- the glue -- -- --
parseYaml = parseMap <|> parseList <|> parseScalar <|> parseJsonLike

-- `parseIndented` is "pushed" as much as possible into the parser chains
-- without breaking stuff 
parseJsonLike = parseJsonMap <|> parseJsonList <|> parseKeyValueJsonWrapper <|> parseScalar
parseScalar =  parseNull <|> parseNumber <|> parseString