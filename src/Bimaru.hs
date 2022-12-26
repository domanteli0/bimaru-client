{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bimaru(
    State(..), emptyState, GameStart, gameStart, checkLength, findDMap, fromDMap,
    fromDList, fromDInteger, fromDString, fromDNull, isDNull, listRepl, matrixRepl, stringToInt, decimalStringToInt,
    parseDocument, Check, renderDocument, render, mkCheck, toggle
) where

import Types
import Data.Char
import Data.List (delete)
import Parser

type Hinted = Coord
type Toggled = Coord
type ColNo = [Int]
type RowNo = [Int]
data State = State [Toggled] [Hinted] ColNo RowNo deriving (Eq, Show)

emptyState :: State
emptyState = State [] [] [] []

--GAMESTART
data GameStart = GameStart ColNo RowNo
instance FromDocument GameStart where
    fromDocument  doc = do
        colNo <- fromDMap doc >>= findDMap "occupied_cols" >>= fromDList >>= mapM fromDInteger >>= checkLength
        rowNo <- fromDMap doc >>= findDMap "occupied_rows" >>= fromDList >>= mapM fromDInteger >>= checkLength
        return $ GameStart colNo rowNo

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

gameStart :: State -> GameStart -> State
gameStart _ (GameStart colNo rowNo) = State [] [] colNo rowNo


--RENDER GAME BOARD
listRepl :: [a] -> Int -> a -> [a]
listRepl l ix el = front ++ back
    where
        splt  = splitAt ix l
        front = fst splt
        back  = el : tail (snd splt)

matrixRepl :: [[a]] -> Int -> Int -> a -> [[a]]
matrixRepl m col' row' el = front ++ back
    where
        splt = splitAt row' m
        l = head $ snd splt
        l' = listRepl l col' el
        front = fst splt
        back = l' : tail (snd splt)

render :: State -> String
render (State ts hs c r) = concat mtrx'' ++ cols
    where
        cols = foldl (\acc colNr -> acc ++ show colNr ++ " " ) "" c
        mtr = zip r $ repeat ". . . . . . . . . . "
        mtrx = map (\(rowNr, str) -> str ++ show rowNr ++ "\n") mtr
        mtrx' = addXH mtrx hs 'H'
        mtrx'' = addXH mtrx' ts 'X'
        addXH :: [[Char]] -> [Coord] -> Char -> [[Char]]
        addXH m cord char = foldl (toggleHint char) m cord
        toggleHint char mat (Coord x y) = matrixRepl mat (x*2) y char


--CHECK
mkCheck :: State -> Check
mkCheck (State ts _ _ _) = Check ts

--TOGGLE COORDS
toggle :: State -> [String] -> State
toggle (State ts _ c r) stringS =
    -- error $ concat stringS
    State (func stringS ts) [] c r 
    where
        func :: [String] -> [Coord] -> [Coord]
        func [] t = t
        func [_] _ = error "Uneven number of arguments"
        func (c':r':xs) t = do
            let new = Coord (toInt c') (toInt r')
            if new `elem` t then func xs (delete new t) else func xs (new : t)

        toInt = decimalStringToInt

stringToInt:: Int -> String -> Int
stringToInt base digits = sign * foldl acc 0 (concatMap digToInt digits1)
    where
        splitSign ('-' : ds) = (-1, ds)
        splitSign ('+' : ds) = ( 1  , ds)
        splitSign ds         = ( 1  , ds)
        (sign, digits1)      = splitSign digits
        digToInt c
            | isDigit c = [ord c - ord '0']
            | isAsciiUpper c =  [ord c - ord 'A' + 10]
            | isAsciiLower c =  [ord c - ord 'a' + 10]
            | otherwise = []
        acc i1 i0= i1 * base + i0

decimalStringToInt:: String -> Int
decimalStringToInt = stringToInt 10


--PARSE YAML
parseDocument :: String -> Either String Document
parseDocument = parse

instance ToDocument Check where
    toDocument a = DMap [("coords", docListToDoc $ deconstructListOfCoord $ checkToList a)]

checkToList :: Check -> [Coord]
checkToList (Check c) = c

docListToDoc :: [Document] -> Document
docListToDoc = DList

deconstructListOfCoord :: [Coord] -> [Document]
deconstructListOfCoord  = map deconstructCoord

deconstructCoord :: Coord -> Document
deconstructCoord (Coord c r) = DMap [("col", DInteger c), ("row", DInteger r)]

--RENDER TO YAML
renderDocument :: Document -> String
renderDocument doc = renderDoc doc 0
    where
        renderDoc :: Document -> Int -> String
        renderDoc DNull _ = "null\n"
        renderDoc (DString s) _ = '\'' : s ++ "'\n"
        renderDoc (DInteger i) _ = show i ++ "\n"
        renderDoc (DList []) level = l level ++ "[]\n"
        renderDoc (DList ls) level = do
            foldl func  "" ls
                where
                    func acc d = acc ++
                        l level ++
                        listStr d ++
                        renderDoc d (level + 1)
        renderDoc (DMap []) level = l level ++ "{}\n"
        renderDoc (DMap m) level = do
            foldl func "" m
                where
                    func :: String -> (String, Document) -> String
                    func acc ("", doc1) = acc ++
                        l level ++
                        "''" ++ mapStr doc1 ++
                        renderDoc doc1 (level + 1)
                    func acc (sstring, doc1) = acc ++
                        l level ++
                        sstring ++ mapStr doc1 ++
                        renderDoc doc1 (level + 1)

        l ind = concat (replicate (2 * ind) " ")

        listStr d = if isDCol d then "-\n" else "- "
        mapStr d = if isDCol d then ":\n" else ": "
        isDCol :: Document -> Bool
        isDCol (DList _) = True
        isDCol (DMap _)  = True
        isDCol __        = False