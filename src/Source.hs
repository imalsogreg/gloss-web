{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Source where

import Prelude hiding (catch)

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Crypto.Hash.MD5
import Data.IORef
import GHC.Exts (unsafeCoerce#)
import Graphics.Gloss
import System.Directory
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

#endif


{-|
    Returns a possibly cached compile result.  The map in the first parameter
    is the cache, which should be different for different expected variables
    and types
-}
getCompileResult :: CacheMap (WorldType, ByteString) CompileResult
                 -> WorldType
                 -> String
                 -> Either ByteString ByteString
                 -> IO (ByteString, ByteString, CompileResult)
getCompileResult cmap typ expr inp = do
    (dig, src, r) <- case inp of
        Left dig -> do
            e  <- doesFileExist (fname dig)
            if not e then return ("", "", ([], Nothing)) else do
                mr <- getCached cmap (typ, dig)
                src <- B.readFile (fname dig)
                case mr of
                    Nothing -> (dig,src,) <$> cache cmap (typ, dig) (result dig)
                    Just r  -> return (dig, src, r)
        Right src -> do
            let dig = hash src
            (dig,src,) <$> (cache cmap (typ, dig) $ do
                B.writeFile (fname dig) src
                result dig)
    keepAlive cmap (typ, dig) 30
    return (dig, src, r)
  where
    fname digest  = "tmp/" ++ base64FileName digest ++ ".hs"
    result digest = compile expr (fname digest)

{-|
    Runs an action in the 'Ghc' monad, and automatically collects error
    messages.  There are multiple ways error messages get reported, and
    it's a bit of tricky trial-and-error to handle them all uniformly, so
    this function abstracts that.
-}
doWithErrors :: GHC.Ghc (Maybe CompiledWorld) -> IO CompileResult
doWithErrors action = do
    codeErrors <- newIORef []
    protectHandlers $ catch (wrapper codeErrors) $ \ (e :: SomeException) -> do
        errs <- readIORef codeErrors
        return (errs, Nothing)
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
    fixupErrors errs (Just x) = fmap (, Just x)  (readIORef errs)
    fixupErrors errs Nothing  = fmap (, Nothing) (readIORef errs)


{-|
    Compile the module in the given file name, and return the named value
    with the given type.  If the type passed in doesn't match the way the
    result is used, the server process will likely segfault.
-}
compile :: String -> FilePath -> IO CompileResult
compile expr fn = doWithErrors $ do
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags {
        GHC.ghcMode = GHC.CompManager,
        GHC.ghcLink = GHC.LinkInMemory,
        GHC.hscTarget = GHC.HscInterpreted,
        GHC.safeHaskell = GHC.Sf_Safe,
        GHC.packageFlags = [GHC.TrustPackage  "gloss",
                            GHC.ExposePackage "gloss-web-adapters",
                            GHC.ExposePackage "random" ]
        }
    GHC.setSessionDynFlags (GHC.dopt_set dflags' GHC.Opt_PackageTrust)
    target <- GHC.guessTarget fn Nothing
    GHC.setTargets [target]
    r <- fmap GHC.succeeded (GHC.load GHC.LoadAllTargets)
    case r of
        True -> do
            mods <- GHC.getModuleGraph
            let mainMod = GHC.ms_mod (head mods)
            GHC.setContext [ GHC.IIModule mainMod,
                             GHC.IIDecl (qualifiedImportDecl "GlossAdapters" "G") ]
            v <- GHC.compileExpr expr
            return (Just (unsafeCoerce# v))
        False -> return Nothing

qualifiedImportDecl m a = (GHC.simpleImportDecl (GHC.mkModuleName m)) {
    GHC.ideclQualified = True,
    GHC.ideclAs = Just (GHC.mkModuleName a)
    }