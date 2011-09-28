{-# LANGUAGE TupleSections #-}

{-|
    Implementation of a map that caches values but does not keep them
    indefinitely.  This is not referentially transparent, so the map lives in
    the IO monad.

    Values are retained in the map when the value is still reachable elsewhere
    in the system.  In addition, values can be forced to remain reachable for
    additional time by using 'keepAlive', which stores a strong reference to
    the value for a limited time, thereby ensuring that the value remains in
    the cache.

    A typical client will use either 'cache' or 'cacheNew', but not both.
-}
module CacheMap (
    CacheMap,
    newCacheMap,
    cache,
    cacheNew,
    getCached,
    keepAlive
    ) where

import Control.Concurrent
import Control.Monad
import System.Random
import System.Mem.Weak
import Data.Time.Clock.POSIX

import           Data.Map (Map)
import qualified Data.Map as M

import           Data.PSQueue (PSQ)
import qualified Data.PSQueue as P


{-|
    A wrapper that holds a reference to a value, but acts just like a
    timestamp.  The purpose of such an object is just to artificially keep a
    reference alive.
-}
data KeepAlive v = KA v POSIXTime

instance Eq (KeepAlive v) where
    KA _ a == KA _ b = a == b

instance Ord (KeepAlive v) where
    compare (KA _ a) (KA _ b) = compare a b


{-|
    A map that caches values but does not keep them indefinitely.  Instead, it
    stores values as long as they are either retained by other garbage
    collection roots, or for explicitly requested keep-alive periods.
-}
newtype CacheMap k v = CacheMap (MVar (CacheTables k v))

data CacheTables k v = CT {
    ctVals  :: Map k (Weak v),
    ctForce :: PSQ k (KeepAlive v)
    }


{-|
    Creates a new, initially empty, 'CacheMap'.
-}
newCacheMap :: Ord k => IO (CacheMap k v)
newCacheMap = fmap CacheMap (newMVar (CT M.empty P.empty))


{-|
    Convenience wrapper for looking up possibly missing values in a map whose
    values are weak pointers.
-}
lookupWeak :: Ord k => Map k (Weak v) -> k -> IO (Maybe v)
lookupWeak vs k = maybe (return Nothing) deRefWeak (M.lookup k vs)


{-|
    Arranges to keep the requested value alive for a period of time, regardless
    of whether it's retained by the rest of the application.  This may be
    useful, for example, if the application is not currently servicing any
    requests that involve the value, but suspects that new requests may be
    arriving and wants to leave the value around for a period of time in case
    they do.
-}
keepAlive :: Ord k => CacheMap k v -> k -> Int -> IO ()
keepAlive (CacheMap tbl) k t = modifyMVar_ tbl $ \(CT vs fs) -> do
    mv <- lookupWeak vs k
    case mv of
        Nothing -> return (CT vs fs)
        Just v  -> keepAlive' (CacheMap tbl) (CT vs fs) k v t


{-|
    This is the guts of setting up a keep-alive timer on a value.
-}
keepAlive' :: Ord k
           => CacheMap k v
           -> CacheTables k v
           -> k -> v -> Int
           -> IO (CacheTables k v)
keepAlive' (CacheMap tbl) (CT vs fs) k v t = do
    t0 <- getPOSIXTime
    when (P.null fs) $ forkIO (reaper (CacheMap tbl)) >> return ()
    return (CT vs (P.insert k (KA v (t0 + fromIntegral t)) fs))


{-|
    A reaper thread is spawned whenever there are active keep-alives.  It's a
    thread that watches the keep-alive queue and removes values from it when
    their timers expire.
-}
reaper :: Ord k => CacheMap k v -> IO ()
reaper (CacheMap var) = do
    t    <- getPOSIXTime
    wait <- modifyMVar var $ \(CT vs fs) -> case P.findMin fs of
        Nothing                            -> return (CT vs fs, Nothing)
        Just (_ P.:-> KA _ t') | t < t'    -> return (CT vs fs, Just (t' - t))
                               | otherwise -> return (CT vs (P.deleteMin fs), Just 0)
    case wait of
        Nothing -> do return ()
        Just t  -> do when (t > 0) $ threadDelay (round (1000000 * t))
                      reaper (CacheMap var)


{-|
    Retrieves a value, getting the cached copy if available, or building a new
    one with the given 'IO' action if not.
-}
cache :: Ord k => CacheMap k v -> k -> IO v -> IO v
cache (CacheMap var) k gen = modifyMVar var $ \(CT vs fs) -> do
    mv <- lookupWeak vs k
    (vs', v) <- case mv of
        Nothing -> do v   <- gen
                      vp  <- makeValue (CacheMap var) k v
                      return (M.insert k vp vs, v)
        Just v  -> do return (vs, v)
    return (CT vs' fs, v)


{-|
    Adds a brand new value to the cache, with a key that's randomly chosen but
    guaranteed not to be currently in use.
-}
cacheNew :: (Random k, Ord k) => CacheMap k v -> v -> IO k
cacheNew (CacheMap var) v = modifyMVar var $ \(CT vs fs) -> do
    k  <- newKey vs
    vp <- makeValue (CacheMap var) k v
    return (CT (M.insert k vp vs) fs, k)


{-|
    Returns a value that's already in the cache, or 'Nothing' if it is not
    there.
-}
getCached :: Ord k => CacheMap k v -> k -> IO (Maybe v)
getCached (CacheMap var) k = withMVar var $ \(CT vs _) -> lookupWeak vs k


{-|
    Generates a new key not in use in the given map.
-}
newKey :: (Random k, Ord k) => Map k v -> IO k
newKey m = do
    k <- randomIO
    if k `M.member` m then newKey m else return k


{-|
    Creates a weak pointer, with a finalizer that removes it from the map.
-}
makeValue (CacheMap var) k v = mkWeakPtr v $ Just $ do
    modifyMVar_ var $ \(CT vs fs) -> return (CT (M.delete k vs) (P.delete k fs))

