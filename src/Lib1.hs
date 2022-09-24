{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.Char
import Data.List (delete)
-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
type Hinted = Coord
type Toggled = Coord
type ColNo = [Int]
type RowNo = [Int]
type HintNo = Int
data State = State [Toggled] [Hinted] ColNo RowNo HintNo

instance Show State where
    show (State ts hs c r h) = "Toggled: " ++ show ts ++ "\nHinted: " ++ show hs ++ "\nCol: " ++ show c ++ "\nRow: " ++ show r ++ "\nNo of hints: " ++ show h

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [] [] [] [] 0

-- IMPLEMENT
-- This adds game data to initial state
-- gameStart :: State -> Document -> State
gameStart :: State -> Document -> State
gameStart _ doc = State [] [] colNo rowNo hintNo
    where
        hintNo = fromDInteger (findDMap "number_of_hints" dMap)
        rowNo = map fromDInteger $ fromDList (findDMap "occupied_rows" dMap)
        colNo = map fromDInteger $ fromDList (findDMap "occupied_cols" dMap)
        dMap = fromDMap doc

findDMap :: String -> [(String, Document)] -> Document
findDMap key ((dKey, doc):xs) = if dKey == key then doc else findDMap key xs
findDMap _ _ = error "Unable to find value with specified key"

fromDMap :: Document -> [(String, Document)]
fromDMap (DMap m) = m
fromDMap _ = error "This Document is not a DMap"
fromDList :: Document -> [Document]
fromDList (DList l) = l
fromDList _ = error "This Document is not a DList"
fromDInteger :: Document -> Int
fromDInteger (DInteger i) = i
fromDInteger _ = error "This Document is not a DInteger"
fromDString :: Document -> String
fromDString (DString i) = i
fromDString _ = error "This Document is not a DString"
fromDNull :: Document -> Maybe a
fromDNull DNull = Nothing
fromDNull _ = error "This Document is not a DNull"
isDNull :: Document -> Bool
isDNull DNull = True
isDNull _ = False

-- repl :: [List] -> Int -> Int -> a -> [List]
-- repl matrix j i elem =
    -- where
listRepl :: [a] -> Int -> a -> [a]
listRepl l ix el = front ++ back
    where
        splt  = splitAt ix l
        front = fst splt
        back  = el : tail (snd splt)

matrixRepl :: [[a]] -> Int -> Int -> a -> [[a]]
matrixRepl m row' col' el = front ++ back
    where
        splt = splitAt row' m
        l = head $ snd splt
        l' = listRepl l col' el
        front = fst splt
        back = l' : tail (snd splt)

-- IMPLEMENT
-- renders your game board
render :: State -> String
render (State ts hs c r h) = concat mtrx' ++ cols
    where
        cols = foldl (\acc colNr -> show colNr ++ " " ++ acc ) "" c
        -- mtrx_ = array
        mtrx = replicate 10 ". . . . . . . . . .\n"
        mtrx' = matrixRepl mtrx 5 10 'X'
-- render (State ts hs c r h) = show
-- render :: [[Char]] -> [Char]
-- render a = foldr (\el acc -> acc:el) "" a

-- IMPLEMENT
-- Make check from current state
-- Perduoti visus toggle'us
mkCheck :: State -> Check
mkCheck (State ts _ _ _ _) = Check ts

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
-- toggle :: State -> [String] -> State
toggle :: State -> [String] -> State
toggle (State ts hs c r h) stringS = State ts' hs c r h
    where
        newToggle = Coord col' row'
            where
                col'= decimalStringToInt (head stringS)
                row' = decimalStringToInt (last stringS)
        ts' = if newToggle `elem` ts then delete newToggle ts else newToggle : ts

--Parsing String to Int
stringToInt:: Int -> String -> Int
stringToInt base digits
    = sign * foldl acc 0 (concatMap digToInt digits1)
      where
      splitSign ('-' : ds) = (-1, ds)
      splitSign ('+' : ds) = ( 1  , ds)
      splitSign ds         = ( 1  , ds)
      (sign, digits1)      = splitSign digits
      digToInt c
          | isDigit c
              = [ord c - ord '0']
          | isAsciiUpper c
              =  [ord c - ord 'A' + 10]
          | isAsciiLower c
              =  [ord c - ord 'a' + 10]
          | otherwise = []
      acc i1 i0= i1 * base + i0

decimalStringToInt:: String -> Int
decimalStringToInt = stringToInt 10

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State ts _ c r h) hintDoc = State ts (hs coordDoc []) c r h
    where
        coordDoc = findDMap "coords" (fromDMap hintDoc)
        hs :: Document -> [Coord] -> [Coord]
        hs doc crds = do
            -- if `tail` in the next document only has `DNull` then return the last coordinates
            if isDNull nextDoc then  Coord getCol getRow : crds
            -- else recursively parse other coordinates
            else hs nextDoc $ Coord getCol getRow : crds
            where
                coordDoc' = fromDMap $ findDMap "head" (fromDMap doc)
                nextDoc = findDMap "tail" (fromDMap doc)
                getCol = numFromDoc coordDoc' "col"
                getRow = numFromDoc coordDoc' "row"
                numFromDoc :: [(String, Document)] -> String -> Int
                numFromDoc doc_ str = fromDInteger $ findDMap str doc_