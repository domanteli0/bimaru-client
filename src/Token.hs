module Token(Token(..), tokenizeYaml, isTokenScalar, isTokenNewLine, isTokenSpace, toYamlStr) where

import Types(Document(..), ToDocument, toDocument)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Data.List (isPrefixOf)

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

  -- ^ meant to symbolize what nothing was parsed, but shouldn't be an error
  deriving (Show, Eq)

toYamlStr :: Token -> String
toYamlStr (TokenScalarString str) = str
toYamlStr (TokenScalarInt int) = show int
toYamlStr TokenScalarNull = "null"
toYamlStr _ = undefined

isTokenNewLine :: Token -> Bool
isTokenNewLine TokenNewLine = True
isTokenNewLine _ = False


isTokenSpace :: Token -> Bool
isTokenSpace (TokenSpace _) = True
isTokenSpace _              = False

isTokenScalar :: Token -> Bool
isTokenScalar (TokenScalarInt _)    = True
isTokenScalar (TokenScalarString _) = True
isTokenScalar TokenScalarNull       = True
isTokenScalar _                     = False

isTokenDashListItem :: Token -> Bool
isTokenDashListItem TokenDashListItem = True
isTokenDashListItem _ = False

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
    pipeline =
        normalizeDashListItem
      . filter'
      . normalizeWhitespace
      . parseScalarToken
      . joinBetweenScalar
      . joinUnknown
      . tokenizeYaml'
      . ensureNL

    -- removes whitespace after `TokenDashListItem`
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

    -- normalizes lists as such:
    -- '- - 5         '
    -- '- key0: value0'
    -- '  key1: value1'
    -- '- null        '
    --     TO THIS
    -- '-             '
    -- '  -           '
    -- '     5        '
    -- '-             '
    -- '  key0: value0'
    -- '  key1: value1'
    -- '-             '
    -- '  null        '
    -- this eliminates the need to maintain several types of parsers
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
              | s !! 1 == char = ("", drop 2 s)
              | otherwise = do
                let str'' = drop 1 s
                let (striped, left) = span (/= char) str''
                if last striped /= excl then
                  (striped, drop 1 left)
                else
                  let (striped', left') = getStr left char excl;
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
