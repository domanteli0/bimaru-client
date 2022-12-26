module Session
where
import Types
import Control.Concurrent.STM
import Data.Monoid ()
import qualified Data.HashMap.Strict as HM
--Game session logic
--https://github.com/agrafix/scotty-session/blob/master/Web/Scotty/Session.hs

data Session a
   = Session
   { sess_id         :: Char
   , sess_content    ::[Coord]
   } deriving (Show, Eq)
type SessionJar a = TVar (HM.HashMap Char (Session a))

newtype ScottySM a = ScottySM { _unSessionManager :: SessionJar a }

insertSession :: Session a -> SessionJar a -> IO ()
insertSession sess sessions =
    atomically $ modifyTVar sessions $ \m -> HM.insert (sess_id sess) sess m

getSession :: Char -> SessionJar a -> IO (Maybe (Session a))
getSession sessId sessions =
    do s <- atomically $ readTVar sessions
       return $ HM.lookup sessId s

shipMap :: [Coord]
shipMap = map (uncurry Coord) [(0, 4), (0, 5), (0, 6), (1, 0), (1, 8), (2, 2), (2, 3), (3, 5), (3, 6), (3, 7), (3, 8), (4, 1), (4, 3), (5, 1),(6, 8), (7, 6), (7, 8), (8, 3), (8, 4), (8, 8)]

