module ClientManager where

import Control.Concurrent
import Control.Monad
import System.Random
import Data.Time

import           Data.Map (Map)
import qualified Data.Map as M


type Key = Int


data ClientManager a = Mgr {
    clientMap :: MVar (Map Key (Client a))
    }


data Client a = Client {
    clientVal  :: a,
    lastAccess :: MVar UTCTime
    }


newClientManager :: IO (ClientManager a)
newClientManager = do
    mgr <- fmap Mgr (newMVar M.empty)
    forkIO $ forever $ do
        threadDelay 10000000
        modifyMVar_ (clientMap mgr) $
            fmap M.fromAscList . filterM isValid . M.toAscList
    return mgr
  where
    isValid (_, Client _ var) = do
        t  <- readMVar var
        t1 <- getCurrentTime
        return (t1 `diffUTCTime` t < 30)


newClient :: ClientManager a -> a -> IO Key
newClient (Mgr m) c = modifyMVar m $ go
  where go cmap = do
            k <- randomIO
            if (k `M.member` cmap)
                then go cmap
                else do
                    t    <- getCurrentTime
                    cval <- fmap (Client c) (newMVar t)
                    return (M.insert k cval cmap, k)


getClient :: ClientManager a -> Key -> IO (Maybe (a, IO ()))
getClient (Mgr m) k = withMVar m $ \cmap -> do
    case M.lookup k cmap of
        Nothing           -> do return Nothing
        Just (Client c t) -> do let touch = (swapMVar t =<< getCurrentTime) >> return ()
                                touch
                                return (Just (c, touch))

