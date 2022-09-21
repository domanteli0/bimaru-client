{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
-- import Lib1 (gameStart)
-- import Control.Lens.Internal.Deque (fromList)
-- import Lib1 (gameStart)
-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
-- data State = State [String] -- OG
type Hinted = Coord
type Toggled = Coord
type ColNo = [Int]
type RowNo = [Int]
type HintNo = Int
data State = State [Toggled] [Hinted] ColNo RowNo HintNo
    deriving Show

-- C like struct'as
{-
struct State {
    Check toggled[], // <- tai kas toggle'inta
    Check hinted[]  // <- tai kas gauta iš hint
}
-}

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
        rowNo = map (fromDInteger) $ fromDList (findDMap "occupied_rows" dMap)
        colNo = map (fromDInteger) $ fromDList (findDMap "occupied_cols" dMap)
        dMap = fromDMap doc

findDMap :: String -> [(String, Document)] -> Document
findDMap key [] = error "Unable to find"
findDMap key ((dKey, doc):xs) = if dKey == key then doc else findDMap key xs
fromDMap :: Document -> [(String, Document)]
fromDMap (DMap m) = m
fromDList :: Document -> [Document]
fromDList (DList l) = l
fromDInteger :: Document -> Int
fromDInteger (DInteger i) = i
fromDString :: Document -> String
fromDString (DString i) = i
fromDNull :: Document -> IO a
fromDNull (DNull) = error "Null"

-- IMPLEMENT
-- renders your game board
render :: State -> String
render = show
-- render :: [[Char]] -> [Char]
-- render a = foldr (\el acc -> acc:el) "" a

-- IMPLEMENT
-- Make check from current state
-- Perduoti visus toggle'us
mkCheck :: State -> Check
-- mkCheck _ = Check []
mkCheck _ = Check co
    where
        co = [
            Coord 8 9,
            Coord 7 9,
            Coord 6 9,
            Coord 5 9,

            Coord 5 5,
            Coord 5 6,
            Coord 5 7,

            Coord 3 1,
            Coord 3 2,
            Coord 3 3

            -- Coord
            ]

-- col_left = [Coord 0 0, Coord 5 0, Coord ]

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
-- toggle :: State -> [String] -> State
-- toggle (State ts hs c r) t = print t
toggle (State ts hs c r h) t = State [] [] [] [] 0

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State ts hs c r h) hNo = State [] [] [] [] 0
