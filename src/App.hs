module App where

import Control.Concurrent.MVar
import Data.Time
import Data.Word
import Graphics.Gloss
import Snap.Types
import System.Random
import Text.Templating.Heist

import           Data.Map (Map)
import qualified Data.Map as M

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64

import GlossAdapters
import CacheMap


type Anim        = (Err (Float  -> Picture),    UTCTime, Float -> Picture)
type Sim         = (Err (StdGen -> Simulation), MVar (UTCTime, Simulation))
type RunningGame = (Err (StdGen -> Game)      , MVar (UTCTime, Word64, Game))

type Err a = Either [String] a

data App = App {
    appHeist               :: TemplateState Snap,
    appAnimations          :: CacheMap Int Anim,
    appSimulations         :: CacheMap Int Sim,
    appGames               :: CacheMap Int RunningGame,
    appCompiledPictures    :: CacheMap ByteString (Err Picture),
    appCompiledAnimations  :: CacheMap ByteString (Err (Float -> Picture)),
    appCompiledSimulations :: CacheMap ByteString (Err (StdGen -> Simulation)),
    appCompiledGames       :: CacheMap ByteString (Err (StdGen -> Game))
    }


newApp :: TemplateState Snap -> IO App
newApp heist = do
    animMgr     <- newCacheMap
    simMgr      <- newCacheMap
    gameMgr     <- newCacheMap
    cpic        <- newCacheMap
    canim       <- newCacheMap
    csim        <- newCacheMap
    cgame       <- newCacheMap
    return (App heist animMgr simMgr gameMgr cpic canim csim cgame)


{-|
    Base64 encodes a ByteString, and forms a filename from it.  Since this is
    a file name, we need to use '-' intead of a slash.  This is used in several
    places, so it's in App so that it is easy to import anywhere.
-}
base64FileName :: ByteString -> FilePath
base64FileName str = map slashToDash $ BC.unpack $ B64.encode str
    where slashToDash '/' = '-'
          slashToDash c   = c

