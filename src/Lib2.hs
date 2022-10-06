{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check )
import Lib1 (State(..))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check where
    toDocument a = DNull

-- IMPLEMENT
-- ASSIGNEE: DOMANTAS
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument doc = "---\n" ++ renderDoc doc 0
    where
        renderDoc DNull level = "null"
        renderDoc (DString s) level = '\'' : s ++ ['\'']
        renderDoc (DInteger i) level = show i
        renderDoc (DList l) level = error "Implement me"
        renderDoc (DMap m) level = error "Implement me"

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error
gameStart :: State -> Document -> Either String State
gameStart (State ts hs c r h) d = Left "TODO: IMPLEMENT GAMESTART"

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error
hint :: State -> Document -> Either String State
hint (State ts hs c r h) d = Left "TODO: IMPLEMENT HINT"
