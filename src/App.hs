module App where

import Control.Concurrent.MVar
import Data.Time
import Graphics.Gloss
import Snap.Types
import Text.Templating.Heist

import           Data.Map (Map)
import qualified Data.Map as M

import Data.ByteString (ByteString)

import GlossAdapters
import ClientManager


type Anim = (UTCTime, Float -> Picture)
type Sim = MVar (UTCTime, Simulation)

data App = App {
    appHeist               :: TemplateState Snap,
    appAnimations          :: ClientManager Anim,
    appSimulations         :: ClientManager Sim,
    appCompiledPictures    :: MVar (Map ByteString (Either [String] Picture)),
    appCompiledAnimations  :: MVar (Map ByteString (Either [String] (Float -> Picture))),
    appCompiledSimulations :: MVar (Map ByteString (Either [String] Simulation))
    }


newApp :: TemplateState Snap -> IO App
newApp heist = do
    animMgr     <- newClientManager
    simMgr      <- newClientManager
    cpic        <- newMVar M.empty
    canim       <- newMVar M.empty
    csim        <- newMVar M.empty
    return (App heist animMgr simMgr cpic canim csim)

