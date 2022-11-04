{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..))

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = error "NOT IMPLEMENTED"

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart deriving Show
instance FromDocument GameStart where
    fromDocument _ = error "NOT IMPLEMENTED"

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart s _ = s
-- gameStart (State l) d = State $ ("Game started: " ++ show d) : l

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
