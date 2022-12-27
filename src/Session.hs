{-# LANGUAGE OverloadedStrings #-}

module Session
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid
import Control.Arrow

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TEnc
import qualified Data.HashMap.Strict as HM

import Crypto.Random (newGenIO, genBytes, SystemRandom)
import Crypto.Types (ByteLength)
import Data.ByteString.Base64 as B64

import Network.Wai

import Data.Time.Clock
import Data.Time.Format

import Bimaru
import Types
import Web.Scotty.Trans
import Data.IORef


-- data Session a
--    = Session
--    { sess_id         :: T.Text
--    , sess_validUntil :: UTCTime
--    , sess_content    :: State
--    } deriving (Show, Eq)
-- type SessionJar a = IOref (HM.HashMap T.Text (Session a))

-- newtype ScottySM a = ScottySM { _unSessionManager :: SessionJar a }

-- -- | Create a new session manager
-- createSessionManager :: MonadIO m => ScottyT e m (ScottySM a)
-- createSessionManager =
--     do storage <- liftIO $ atomically $ newTVar HM.empty
--        liftIO $ forkIO $ maintainSessions storage
--        return $ ScottySM storage

-- -- | Modify the current users session
-- modifySession :: (MonadIO m, ScottyError e) => ScottySM a -> (State -> State) -> ActionT e m ()
-- modifySession sm@(ScottySM storage) fun =
--     do oldS <- readSession' sm id
--        let newS = oldS { sess_content = fun (sess_content oldS) }
--        liftIO $ insertSession newS storage

-- -- | Read the current users session
-- readSession :: (MonadIO m, ScottyError e) => ScottySM a -> ActionT e m State
-- readSession sm = readSession' sm sess_content

-- readSession' :: (MonadIO m, ScottyError e) => ScottySM a -> (Session a -> b) -> ActionT e m b
-- readSession' (ScottySM storage) fun =
--     do mSession <- loadSession storage
--        case mSession of
--          Just s -> return $ fun s
--          Nothing ->
--              do newS <- liftIO createSession
--                 liftIO $ insertSession newS storage
--                 setCookie newS
--                 return $ fun newS

-- sessionTTL :: NominalDiffTime
-- sessionTTL = 36000 -- in seconds

-- sessionIdEntropy :: ByteLength
-- sessionIdEntropy = 12

-- createSession :: IO (Session a)
-- createSession =
--     do gen <- g
--        sid <- case genBytes sessionIdEntropy gen of
--                  Left err -> fail $ show err
--                  Right (x, _) -> return $ B64.encodeBase64 x
--        now <- getCurrentTime
--        let validUntil = addUTCTime sessionTTL now
--        return $ Session sid validUntil emptyState
--     where g = newGenIO :: IO SystemRandom

-- insertSession :: Session a -> SessionJar a -> IO ()
-- insertSession sess sessions =
--     atomically $ modifyTVar sessions $ \m -> HM.insert (sess_id sess) sess m

-- getSession :: T.Text -> SessionJar a -> IO (Maybe (Session a))
-- getSession sessId sessions =
--     do s <- atomically $ readTVar sessions
--        return $ HM.lookup sessId s

-- maintainSessions :: SessionJar a -> IO ()
-- maintainSessions sessions =
--   do now <- getCurrentTime
--      let stillValid sess = sess_validUntil sess > now
--      atomically $ modifyTVar sessions $ \m -> HM.filter stillValid m
--      threadDelay 1000000
--      maintainSessions sessions

-- setCookie :: (Monad m, ScottyError e) => Session a -> ActionT e m ()
-- setCookie sess =
--     do let formattedExp = TL.pack $ formatTime defaultTimeLocale "%a, %d %b %Y %X %Z" (sess_validUntil sess)
--        setHeader "Set-Cookie" $ "sid=" <> TL.fromStrict (sess_id sess) <> "; Path=/; Expires=" <> formattedExp

-- loadSession :: (MonadIO m, ScottyError e) => SessionJar a -> ActionT e m (Maybe (Session a))
-- loadSession sessions =
--     do req <- request
--        liftIO $ getUserSession req sessions

-- getUserSession :: Request -> SessionJar a -> IO (Maybe (Session a))
-- getUserSession req sessions =
--     case lookup "cookie" (requestHeaders req) >>=
--          lookup "sid" . parseCookies . TEnc.decodeUtf8 of
--       Just sid -> lookupSession sid
--       Nothing -> return Nothing
--     where lookupSession sid = getSession sid sessions

-- parseCookies :: T.Text -> [(T.Text, T.Text)]
-- parseCookies = map parseCookie . T.splitOn ";" . T.concat . T.words
--     where parseCookie = first T.init . T.breakOnEnd "="



