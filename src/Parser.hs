{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use !!" #-}
{-# LANGUAGE InstanceSigs #-}

module Parser(parse, Token(..), tokenizeYaml) where

import Types(Document(..))
import Control.Applicative
import Token

-- - Raktai neturės "specialių" simbolių, o tik raides
-- - Eilutės galimos tik raidės, skaitmenys ir tarpas
-- - Eilutės maksimalus ilgis 16
-- - Rakto maksimalus ilgis 10

newtype Parser a = Parser
  { runParser :: [Token] -> Either String (a, [Token])
  }

-- type ParserRefac a = ExceptT (MyState _ _) (Either a, [Token])

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
  empty = Parser $ const $ Left "ERR_00A: No parser"
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return x = Parser $ \input -> Right (x, input)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p1 >>= f =
    Parser $ \input -> do
      (res, ts) <- runParser p1 input
      runParser (f res) ts

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
        func [] = Left "ERR_02A: Empty token list"


tokenP :: Token -> Parser Token
tokenP = tokenPFac dumbCmp

scalarTokenP :: Parser Token
scalarTokenP = tokenPFac (\_ t2 -> isTokenScalar t2) $ TokenScalarInt 1

dumbCmp :: Token -> Token -> Bool
dumbCmp (TokenScalarInt _) (TokenScalarInt _) = True
dumbCmp (TokenScalarString _) (TokenScalarString _) = True
dumbCmp (TokenSpace _) (TokenSpace _) = True
dumbCmp t1 t2 = t1 == t2

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
      sep :: Parser Token
      sep = wsOptional *> tokenP TokenCollectionSep <* wsOptional
      one :: Parser Document
      one = wsOptional *> parseJsonLike <* wsOptional

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

unsafeGetTokenSpace :: Token -> Int
unsafeGetTokenSpace (TokenSpace i) = i
unsafeGetTokenSpace _ = undefined

-- parseTokenSpace :: [Token] -> Either String (Token, [Token])
-- parseTokenSpace ts = expectToken (TokenSpace 10) ts

parseTokenSpace :: Parser Token
parseTokenSpace = Parser $ \ts -> expectToken (TokenSpace 10) ts

expectToken :: Token -> [Token] -> Either String (Token, [Token])
expectToken ch [] = Left $ "Empty input: '" ++ show ch ++ " expected"
expectToken t (t':ts) | t `dumbCmp` t' = Right (t', ts)
                      | otherwise = Left $ show t ++ " expected, got: " ++ show (t':ts)

parseList :: Parser Document
parseList = DList <$> parseList'

parseList' :: Parser [Document]
parseList' = do
  (ind, doc) <- first <* nlOptional
  docs <- many (latter ind <* nlOptional)

  return $ doc:docs

  where
    first :: Parser (Int, Document)
    first = do
      ind <- parseTokenSpace
      let ind' = unsafeGetTokenSpace ind

      _ <- tokenP TokenDashListItem <* wsOptional <*  nlOptional
      doc <- parseYaml
      return (ind', doc)

    latter :: Int -> Parser Document
    latter ind = do
      ind' <- parseTokenSpace
      let ind'' = unsafeGetTokenSpace ind'
      _ <- tokenP TokenDashListItem <* wsOptional <* nlOptional
      doc <- parseYaml

      if ind /= ind'' then
        throwErrorP "Mismacthed indentation"
      else
        return doc

throwErrorP :: String -> Parser a
throwErrorP errMsg = Parser $ \_ -> Left errMsg

parseMap :: Parser Document
-- parseMap = Parser $ _a -- [Token] -> Either String (a, [Token])
parseMap = DMap <$> parseMap'

parseMap' :: Parser [(String, Document)]
parseMap' = do
  (ind, doc) <- first <* nlOptional
  docs <- many (latter ind <* nlOptional)

  return $ doc:docs

  where
    first :: Parser (Int, (String, Document))
    first = do
      ind <- parseTokenSpace
      let ind' = unsafeGetTokenSpace ind
      doc <- parseKeyValue
      return (ind', doc)

    latter :: Int -> Parser (String, Document)
    latter ind = do
      ind' <- parseTokenSpace
      let ind'' = unsafeGetTokenSpace ind'
      doc <- parseKeyValue

      if ind /= ind'' then
        throwErrorP "Mismacthed indentation"
      else
        return doc

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
parseJsonLike = parseJsonMap <|> parseJsonList <|> parseKeyValueJsonWrapper <|> parseScalar
parseScalar =  parseNull <|> parseNumber <|> parseString
