{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Source where

import Prelude hiding (catch)

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Crypto.Hash.MD5
import Data.IORef
import GHC.Exts (unsafeCoerce#)
import Graphics.Gloss
import System.FilePath
import System.Random

import           Data.Map (Map)
import qualified Data.Map as M

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64

import qualified GHC        as GHC
import qualified MonadUtils as GHC
import qualified GHC.Paths  as GHC
import qualified Bag        as GHC
import qualified Outputable as GHC
import qualified ErrUtils   as GHC
import qualified HscTypes   as GHC
import qualified DynFlags   as GHC

import App
import CacheMap
import GlossAdapters
import ProtectHandlers

#ifdef PROFILE_SUBST

import ProfileSubst

getPicture :: App -> ByteString -> IO (Err Picture)
getPicture _ _ = return (Right picture)

getAnimation :: App -> ByteString -> IO (Err (Float -> Picture))
getAnimation _ _ = return (Right animation)

getSimulation :: App -> ByteString -> IO (Err Simulation)
getSimulation _ _ = return (Right simulation)

getGame :: App -> ByteString -> IO (Err Game)
getGame _ _ = return (Right game)

#else

getPicture :: App -> ByteString -> IO (Err Picture)
getPicture app src = do
    getCompileResult (appCompiledPictures app)
                     "picture"
                     "Picture"
                     src


getAnimation :: App -> ByteString -> IO (Err (Float -> Picture))
getAnimation app src = do
    getCompileResult (appCompiledAnimations app)
                     "animation"
                     "Float -> Picture"
                     src


getSimulation :: App -> ByteString -> IO (Err Simulation)
getSimulation app src = do
    getCompileResult (appCompiledSimulations app)
                     "Simulation initial step draw"
                     "Simulation"
                     src


getGame :: App -> ByteString -> IO (Err Game)
getGame app src = do
    getCompileResult (appCompiledGames app)
                     "Game initial event step draw"
                     "Game"
                     src

#endif


{-|
    Returns a possibly cached compile result.  The map in the first parameter
    is the cache, which should be different for different expected variables
    and types
-}
getCompileResult :: CacheMap ByteString (Err t)
                 -> String
                 -> String
                 -> ByteString
                 -> IO (Err t)
getCompileResult cmap vname tname src = do
    let digest = hash src
    r <- cache cmap digest $ do
        let fn = "tmp/" ++ base64FileName digest ++ ".hs"
        B.writeFile fn src
        compile vname tname fn
    keepAlive cmap digest 30
    return r


{-|
    Runs an action in the 'Ghc' monad, and automatically collects error
    messages.  There are multiple ways error messages get reported, and
    it's a bit of tricky trial-and-error to handle them all uniformly, so
    this function abstracts that.
-}
doWithErrors :: GHC.Ghc (Maybe a) -> IO (Err a)
doWithErrors action = do
    codeErrors <- newIORef []
    protectHandlers $ catch (wrapper codeErrors) $ \ (e :: SomeException) -> do
        errs <- readIORef codeErrors
        return (Left errs)
  where
    wrapper codeErrors = fixupErrors codeErrors =<< do
        GHC.defaultErrorHandler (logAction codeErrors)
            $ GHC.runGhc (Just GHC.libdir)
            $ GHC.handleSourceError (handle codeErrors)
            $ do
                dflags <- GHC.getSessionDynFlags
                GHC.setSessionDynFlags dflags {
                    GHC.log_action = logAction codeErrors
                    }
                action
    logAction errs _ span style msg =
        let niceError = GHC.showSDoc
                $ GHC.withPprStyle style $ GHC.mkLocMessage span msg
        in  writeErr errs niceError
    writeErr ref err = modifyIORef ref (++ [ err ])
    handle ref se = do
        let errs    = GHC.bagToList (GHC.srcErrorMessages se)
            cleaned = map (GHC.showSDoc . GHC.errMsgShortDoc) errs
        GHC.liftIO $ modifyIORef ref (++ cleaned)
        return Nothing
    fixupErrors errs (Just x) = return (Right x)
    fixupErrors errs Nothing  = fmap Left (readIORef errs)


{-|
    Compile the module in the given file name, and return the named value
    with the given type.  If the type passed in doesn't match the way the
    result is used, the server process will likely segfault.
-}
compile :: String -> String -> FilePath -> IO (Err t)
compile vname tname fn = doWithErrors $ do
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags {
        GHC.ghcMode = GHC.CompManager,
        GHC.ghcLink = GHC.LinkInMemory,
        GHC.hscTarget = GHC.HscInterpreted,
        GHC.safeHaskell = GHC.Sf_Safe,
        GHC.packageFlags = [GHC.TrustPackage "gloss",
                            GHC.ExposePackage "gloss-web-adapters" ]
        }
    GHC.setSessionDynFlags dflags'
    target <- GHC.guessTarget fn Nothing
    GHC.setTargets [target]
    r <- fmap GHC.succeeded (GHC.load GHC.LoadAllTargets)
    case r of
        True -> do
            mods <- GHC.getModuleGraph
            let mainMod = GHC.ms_mod (head mods)
            GHC.setContext [ mainMod ]
                           [ GHC.simpleImportDecl
                               (GHC.mkModuleName "Graphics.Gloss"),
                             GHC.simpleImportDecl
                               (GHC.mkModuleName "GlossAdapters") ]
            v <- GHC.compileExpr $ vname ++ " :: " ++ tname
            return (Just (unsafeCoerce# v))
        False -> return Nothing

