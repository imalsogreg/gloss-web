module App where

import Control.Concurrent.MVar
import Data.Time
import Graphics.Gloss
import Snap.Types
import Text.Templating.Heist

import           Data.Map (Map)
import qualified Data.Map as M

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64

import GlossAdapters
import ClientManager


type Anim        = (UTCTime, Float -> Picture)
type Sim         = MVar (UTCTime, Simulation)
type RunningGame = MVar (UTCTime, Game)

data App = App {
    appHeist               :: TemplateState Snap,
    appAnimations          :: ClientManager Anim,
    appSimulations         :: ClientManager Sim,
    appGames               :: ClientManager RunningGame,
    appCompiledPictures    :: MVar (Map ByteString (Either [String] Picture)),
    appCompiledAnimations  :: MVar (Map ByteString (Either [String] (Float -> Picture))),
    appCompiledSimulations :: MVar (Map ByteString (Either [String] Simulation)),
    appCompiledGames       :: MVar (Map ByteString (Either [String] Game))
    }


newApp :: TemplateState Snap -> IO App
newApp heist = do
    animMgr     <- newClientManager
    simMgr      <- newClientManager
    gameMgr     <- newClientManager
    cpic        <- newMVar M.empty
    canim       <- newMVar M.empty
    csim        <- newMVar M.empty
    cgame       <- newMVar M.empty
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

