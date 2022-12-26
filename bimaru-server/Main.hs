{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Web.Scotty
import Session
import Types
import Bimaru(renderDocument)
import Data.Text.Lazy as Text (pack)

--Main server logic

main :: IO ()
main = do
    scotty 3000 $ do
        get (capture "/newToken") $ do
            yamlText $ DInteger 3
        get (capture "/check") $ do
            yamlText $ DInteger 1
 

yamlText :: Document -> ActionM()
yamlText t = text $ pack $ renderDocument t