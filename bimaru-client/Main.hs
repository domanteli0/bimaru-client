{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, modify, put, StateT )
import Data.ByteString as B ( empty, ByteString )
import Data.Either as E (fromRight)
import qualified Data.List as L
import Data.Text as T ( concat, drop, pack, unpack, Text )
import Data.Text.Encoding.Base64 (decodeBase64)
import Data.Text.IO as TIO ( hPutStrLn, putStrLn )
import Data.List.Split as S ( splitOn )
import Data.Char (isSpace)
import Bimaru
import Types(Check, toDocument, fromDocument, Coord)
import Network.Wreq
    ( post, postWith, defaults, header, responseBody, getWith )
import qualified Network.Wreq as Wreq

import Control.Lens
import System.Exit (exitSuccess, exitFailure)
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import System.Environment (getArgs)
import System.IO (stderr)

import Data.String.Conversions

type Repl a = HaskelineT (StateT (String, State) IO) a

commandShow :: String
commandShow = "show"

commandCheck :: String
commandCheck = "check"

commandToggle :: String
commandToggle = "toggle"

commandNewToken :: String
commandNewToken = "new"

commandExit :: String
commandExit = "exit"

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd c
  | trim c == commandShow = lift get >>= liftIO . Prelude.putStrLn . render . snd
  | trim c == commandCheck = lift get >>= check . (mkCheck . snd) >>= liftIO . Prelude.putStrLn
  | trim c == commandNewToken = newToken >>= liftIO . Prelude.putStr
  | trim c == commandExit = exit "Exited the game."
  | commandToggle `L.isPrefixOf` trim c = do
    case tokens c of
      [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ " expects at leas one argument"
      t -> lift $ modify (\(u, s) -> (u, toggle s (L.drop 1 t)))
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

check :: Check -> Repl String
check c = do
  (url, _) <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = cs $ renderDocument $ toDocument c :: B.ByteString
  resp <- liftIO $ postWith opts (url ++ "/check") body
  pure $ cs $ resp ^. responseBody

newToken :: Repl String
newToken = do
  (url, _) <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  resp <- liftIO $ getWith opts (url ++ "/newToken")
  pure $ cs $ resp ^. responseBody


-- toggle :: [Coord] -> Repl String
-- toggle coords = do
--   (url, _) <- lift get
--   let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
--   let body = cs $ renderDocument $ toDocument coords :: B.ByteString
--   resp <- liftIO $ postWith opts (url ++ "/toggle") body
--   pure $ cs $ resp ^. responseBody


exit :: Text -> Repl ()
exit msg = do
    liftIO $ TIO.putStrLn msg
    liftIO exitSuccess

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = [commandShow, commandCheck, commandToggle, commandNewToken]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  (url, s) <- lift get
  r <- liftIO $ post url B.empty
  let gs = parseDocument (cs (r ^. responseBody)) >>= fromDocument
  case (gs :: Either String GameStart) of
    Left msg -> liftIO $ fatal $ cs msg
    Right d -> do
      lift $ put (url, gameStart s d)
      liftIO $ TIO.putStrLn "Welcome to Bimaru v4. Press [TAB] for available commands list"

fatal :: Text -> IO ()
fatal msg = do
  TIO.hPutStrLn stderr $ T.concat ["ERROR: ", msg]
  exitFailure

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

--stack run bimaru-client -- tokenas
--stack run bimaru-client -- new
main :: IO()
main = do
  args <- getArgs
  case args of
    [token] -> run $ T.pack token
    _ -> fatal "token not provided, expected at least one command argument"

--If you want to start new session, first command line arg must be "new"
run :: T.Text -> IO ()
run token = do
    let url = "localhost:3000/"
    let fullUrl = T.unpack (T.concat ["http://", url, token])
    evalStateT (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) (fullUrl, emptyState)