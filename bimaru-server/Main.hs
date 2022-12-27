{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Web.Scotty
import Session
import Types
import Control.Monad.IO.Class (MonadIO(liftIO))
import Bimaru(renderDocument, State(..), emptyState)
import Data.Text.Lazy (pack)
import Data.Map as M
import Data.IORef (newIORef, modifyIORef, readIORef, IORef)
import Network.HTTP.Types (status404)
import Control.Applicative (Alternative(empty))

--Main server logic
--https://www.stackbuilders.com/blog/getting-started-with-haskell-projects-using-scotty/
main :: IO ()
main = do
    sessions <- newIORef (1 :: Int, mempty :: Map Int State)
    scotty 3000 $ do
        post (capture "/") $ do
            yamlText gameStart

        get (capture "/newToken") $ do
            (id, st) <- liftIO $ readIORef sessions
            liftIO $ modifyIORef sessions $ \(i, state) -> (i + 1, M.insert i emptyState state)
            yamlText $ DInteger id
            
        get (capture ("/:id" ++ "/check")) $ do
           -- sess_id <- param "id"
          --  (id, state) <- liftIO $ readIORef sessions
            yamlText $ DInteger 1
        post (capture ("/:id" ++ "/toggle")) $ do
            sess_id <- param "id"
            --toggled coords
            yamlText $ DInteger sess_id           
--irasyti i state toggle koordinates
--kai check paziureti i toggle coordinates

yamlText :: Document -> ActionM()
yamlText t = text $ pack $ renderDocument t


gameStart :: Document
gameStart = DMap [("occupied_cols",DList [DInteger 1,DInteger 2,DInteger 1,DInteger 3,DInteger 2,DInteger 2,DInteger 3,DInteger 1,DInteger 5,DInteger 0]),("occupied_rows",DList [DInteger 3,DInteger 2,DInteger 2,DInteger 4,DInteger 2,DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 0])]

solution :: [Coord]
solution =[Coord 0 4, Coord 0 5, Coord 0 6, Coord 1 0, Coord 1 8, Coord 2 2, Coord 2 3, Coord 3 5, Coord 3 6, Coord 3 7, Coord 3 8, Coord 4 1, Coord 4 3, Coord 5 1, Coord 6 8, Coord 7 6, Coord 7 8, Coord 8 3, Coord 8 4, Coord 8 8]
