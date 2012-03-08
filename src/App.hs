module App where

import Control.Concurrent.MVar
import Data.Time
import Data.Word
import Graphics.Gloss
import Snap.Core
import System.Random
import Text.Templating.Heist

import           Data.Map (Map)
import qualified Data.Map as M

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64

import GlossAdapters
import CacheMap

type CompileResult = ([String], Maybe CompiledWorld)

data App = App {
    appHeist    :: HeistState Snap,
    appSessions :: CacheMap Int (MVar (UTCTime, Int, World)),
    appPrograms :: CacheMap (WorldType, ByteString) CompileResult
    }


newApp :: HeistState Snap -> IO App
newApp heist = do
    sessions <- newCacheMap
    programs <- newCacheMap
    return (App heist sessions programs)


{-|
    Base64 encodes a ByteString, and forms a filename from it.  Since this is
    a file name, we need to use '-' intead of a slash.  This is used in several
    places, so it's in App so that it is easy to import anywhere.
-}
base64FileName :: ByteString -> FilePath
base64FileName str = map slashToDash $ BC.unpack $ B64.encode str
    where slashToDash '/' = '-'
          slashToDash c   = c

