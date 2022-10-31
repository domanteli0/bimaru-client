{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib2(renderDocument, hint, gameStart, hs) where

import Types
import Lib1 (State(..))

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
                    Right q4 -> case checkLength q4 of
                        Left msg5 -> Left msg5
                        Right q5 -> Right q5

    rowNo <- case fromDMap doc of
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "occupied_rows" q1 of
            Left msg2 -> Left msg2
            Right q2 -> case fromDList q2 of
                Left msg3 -> Left msg3
                Right q3 -> case mapM fromDInteger q3 of
                    Left msg4 -> Left msg4
                    Right q4 -> case checkLength q4 of
                        Left msg5 -> Left msg5
                        Right q5 -> Right q5

    hintNo <- case fromDMap doc of
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "number_of_hints" q1 of
            Left msg2 -> Left msg2
            Right q2 -> case fromDInteger q2 of
                Left msg3 -> Left msg3
                Right q3 -> Right q3

    return $ State [] [] colNo rowNo hintNo

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

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error
hint :: State -> Document -> Either String State
hint (State ts _ c r h) hintDoc = do

    coordDoc <- case fromDMap hintDoc of
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "coords" q1 of
            Left msg2 -> Left msg2
            Right q2 -> Right q2

    temp <- hs coordDoc []

    return $ State ts temp c r h

hs :: Document -> [Coord] -> Either String [Coord]
hs doc crds = do
    -- nextDoc = findDMap "tail" (fromDMap doc)
    nextDoc <- case fromDMap doc of
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "tail" q1 of
            Left msg2 -> Left msg2
            Right q2 -> Right q2

    coordDoc' <- case fromDMap doc of
        Left msg1 -> Left msg1
        Right q1 -> case findDMap "head" q1 of
            Left msg2 -> Left msg2
            Right q2 -> case fromDMap q2 of
                Left msg3 -> Left msg3
                Right q3 -> Right q3

    -- getCol = fromDInteger $ findDMap "col" coordDoc' 
    getCol <- case findDMap "col" coordDoc' of
        Left msg1 -> Left msg1
        Right q1 -> case fromDInteger q1 of
            Left msg2 -> Left msg2
            Right q2 -> Right q2


    -- getCol = fromDInteger $ findDMap "row" coordDoc' 
    getRow <- case findDMap "row" coordDoc' of
        Left msg1 -> Left msg1
        Right q1 -> case fromDInteger q1 of
            Left msg2 -> Left msg2
            Right q2 -> Right q2

    let temp = Coord getCol getRow : crds

    -- if `tail` in the next document only has `DNull` then return the last coordinates
    if isDNull nextDoc then Right temp
    -- else recursively parse other coordinates
    else do
        hs nextDoc $ Coord getCol getRow : crds



        