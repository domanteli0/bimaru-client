{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check )
import Lib1 (State(..), render)
import Data.Aeson (Value(Bool))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check where
    toDocument :: Check -> Document
    toDocument a = DNull

-- IMPLEMENT
-- ASSIGNEE: DOMANTAS
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument doc = renderDoc doc 0
    where
        renderDoc :: Document -> Int -> String
        renderDoc DNull _ = "null\n"
        renderDoc (DString s) _ = '\'' : s ++ "'\n"
        renderDoc (DInteger i) _ = show i ++ "\n"
        renderDoc (DList ls) level = do
            foldl func  "" ls
                where
                    func acc d = acc ++
                        l level ++
                        listStr d ++
                        renderDoc d (level + 1)

        renderDoc (DMap m) level = do
            foldl func "" m
                where
                    func :: String -> (String, Document) -> String
                    func acc tup = acc ++
                        l level ++
                        fst tup ++ mapStr (snd tup) ++
                        renderDoc (snd tup) (level + 1)
                    -- func acc tup = error "TO IMPLEMENT"

        l ind = concat (replicate (2 * ind) " ")

        listStr d = if isDCol d then "-\n" else "- "
        mapStr d = if isDCol d then ":\n" else ": "
        isDCol :: Document -> Bool
        -- isDCol (DList _) = True
        -- isDCol (DMap _)  = True
        -- isDCol __        = False
        isDCol d = isDMap d || isDList d
        isDMap (DMap _) = True
        isDMap _        = False
        isDList (DList _) = True
        isDList _         = False
        -- isDScalar d = (isDMap d) || (isDMap d)
-- This adds game data to initial state
-- Errors are reported via Either but not error
gameStart :: State -> Document -> Either String State
gameStart (State ts hs c r h) d = Left "TODO: IMPLEMENT GAMESTART"

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error
hint :: State -> Document -> Either String State
hint (State ts hs c r h) d = Left "TODO: IMPLEMENT HINT"
