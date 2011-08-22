{-# LANGUAGE MagicHash                 #-}

module Source where

import Control.Concurrent.MVar
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
import GlossAdapters


getPicture :: App -> ByteString -> IO (Either [String] Picture)
getPicture app src = do
    getCompileResult (appCompiledPictures app)
                     "picture"
                     "Picture"
                     src


getAnimation :: App -> ByteString -> IO (Either [String] (Float -> Picture))
getAnimation app src = do
    getCompileResult (appCompiledAnimations app)
                     "animation"
                     "Float -> Picture"
                     src


getSimulation :: App -> ByteString -> IO (Either [String] Simulation)
getSimulation app src = do
    getCompileResult (appCompiledSimulations app)
                     "Simulation initial step draw"
                     "Simulation"
                     src


base64FileName :: ByteString -> String
base64FileName str = map slashToDash $ BC.unpack $ B64.encode str
    where slashToDash '/' = '-'
          slashToDash c   = c


getCompileResult :: MVar (Map ByteString (Either [String] t))
                 -> String
                 -> String
                 -> ByteString
                 -> IO (Either [String] t)
getCompileResult var vname tname src = modifyMVar var $ \m -> do
        let digest = hash src
        case M.lookup digest m of
            Nothing  -> do
                let fn = "tmp/" ++ base64FileName digest ++ ".hs"
                B.writeFile fn src
                res <- compile vname tname fn
                return (M.insert digest res m, res)
            Just res -> return (m, res)


compile :: String -> String -> FilePath -> IO (Either [String] t)
compile vname tname fn = fixupErrors =<< do
    codeErrors <- newIORef []
    GHC.defaultErrorHandler (addErrorTo codeErrors)
        $ GHC.runGhc (Just GHC.libdir)
        $ GHC.handleSourceError (handle codeErrors) $ do
            dflags <- GHC.getSessionDynFlags
            GHC.setSessionDynFlags $ dflags {
                GHC.ghcMode = GHC.CompManager,
                GHC.ghcLink = GHC.LinkInMemory,
                GHC.hscTarget = GHC.HscInterpreted,
                GHC.safeHaskell = GHC.Sf_Safe,
                GHC.packageFlags = [GHC.TrustPackage "gloss",
                                    GHC.ExposePackage "gloss-web-adapters" ],
                GHC.log_action = addErrorTo codeErrors
                }
            target <- GHC.guessTarget fn Nothing
            GHC.setTargets [target]
            r <- fmap GHC.succeeded (GHC.load GHC.LoadAllTargets)
            case r of
                True -> do
                    mods <- GHC.getModuleGraph
                    GHC.setContext [ GHC.ms_mod (head mods) ]
                                   [ GHC.simpleImportDecl
                                       (GHC.mkModuleName "Graphics.Gloss"),
                                     GHC.simpleImportDecl
                                       (GHC.mkModuleName "GlossAdapters") ]
                    v <- GHC.compileExpr $ vname ++ " :: " ++ tname
                    return (Right (unsafeCoerce# v))
                False -> return (Left codeErrors)
  where
    handle ref se = do
        let errs    = GHC.bagToList (GHC.srcErrorMessages se)
            nice e  = GHC.showSDoc (GHC.errMsgShortDoc e)
            cleaned = map nice errs
        GHC.liftIO $ modifyIORef ref (++ cleaned)
        return (Left ref)
    addErrorTo ref _ span style msg =
        let niceError = GHC.showSDoc
                $ GHC.withPprStyle style $ GHC.mkLocMessage span msg
        in  modifyIORef ref (++ [ niceError ])
    fixupErrors (Right x) = return (Right x)
    fixupErrors (Left  x) = fmap Left (readIORef x)

