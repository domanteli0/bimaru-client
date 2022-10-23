{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib2(renderDocument, hint, gameStart) where

import Types
import Lib1 (State(..), render)
import Data.Either

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check where
    toDocument :: Check -> Document
    toDocument a = docListToDoc $ deconstructListOfCoord $ checkToList a

checkToList :: Check -> [Coord]
checkToList (Check a) = a

docListToDoc :: [Document] -> Document
docListToDoc = DList

--Turns Check into [Document]
deconstructListOfCoord :: [Coord] -> [Document]
deconstructListOfCoord  = map deconstructCoord 

--Takes coord and turn to Document
deconstructCoord :: Coord -> Document
deconstructCoord (Coord a b) = DList [DInteger a, DInteger b]

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

        l ind = concat (replicate (2 * ind) " ")

        listStr d = if isDCol d then "-\n" else "- "
        mapStr d = if isDCol d then ":\n" else ": "
        isDCol :: Document -> Bool
        isDCol (DList _) = True
        isDCol (DMap _)  = True
        isDCol __        = False
        
-- This adds game data to initial state
-- Errors are reported via Either but not error
gameStart :: State -> Document -> Either String State
gameStart _ doc = do
    colNo <- case fromDMap doc of 
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "occupied_cols" q1 of
            Left msg2 -> Left msg2
            Right q2 -> case fromDList q2 of
                Left msg3 -> Left msg3
                Right q3 -> case mapM fromDInteger q3 of
                    Left msg4 -> Left msg4
                    Right q4 -> Right q4
                       
    rowNo <- case fromDMap doc of 
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "occupied_rows" q1 of
            Left msg2 -> Left msg2
            Right q2 -> case fromDList q2 of
                Left msg3 -> Left msg3
                Right q3 -> case mapM fromDInteger q3 of
                    Left msg4 -> Left msg4
                    Right q4 -> Right q4
                
    hintNo <- case fromDMap doc of
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "number_of_hints" q1 of
            Left msg2 -> Left msg2
            Right q2 -> case fromDInteger q2 of
                Left msg3 -> Left msg3
                Right q3 -> Right q3

    return $ State [] [] colNo rowNo hintNo

findDMap :: String -> [(String, Document)] -> Either String Document
findDMap key ((dKey, doc):xs) =   if dKey ==  key then Right doc else findDMap key xs
findDMap _ _ = Left "Unable to find value with specified key"

fromDMap :: Document -> Either String [(String, Document)]
fromDMap (DMap m) = Right m
fromDMap _ = Left "This Document is not a DMap"

fromDList :: Document -> Either String [Document]
fromDList (DList l) = Right l
fromDList _ = Left "This Document is not a DList"

fromDInteger :: Document -> Either String Int
fromDInteger (DInteger i) = Right i
fromDInteger _ = Left "This Document is not a DInteger"

fromDString :: Document -> Either String String
fromDString (DString i) = Right i
fromDString _ = Left "This Document is not a DString"

fromDNull :: Document -> Either String (Maybe a)
fromDNull DNull = Right Nothing
fromDNull _ = Left "This Document is not a DNull"

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error
hint :: State -> Document -> Either String State
hint (State ts hs c r h) d = Left "TODO: IMPLEMENT HINT"
