{-# LANGUAGE MagicHash                 #-}

module Source where

import Control.Monad
import Data.IORef
import GHC.Exts (unsafeCoerce#)
import Graphics.Gloss
import System.FilePath
import System.Random

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified GHC        as GHC
import qualified MonadUtils as GHC
import qualified GHC.Paths  as GHC
import qualified Bag        as GHC
import qualified Outputable as GHC
import qualified ErrUtils   as GHC
import qualified HscTypes   as GHC
import qualified DynFlags   as GHC

import GlossAdapters


getPicture :: ByteString -> IO (Either [String] Picture)
getPicture src = do
    r <- getValue "picture" "Picture" src
    return (right (\x -> (unsafeCoerce# x :: Picture)) r)


getAnimation :: ByteString -> IO (Either [String] (Float -> Picture))
getAnimation src = do
    r <- getValue "animation" "Float -> Picture" src
    return (right (\x -> (unsafeCoerce# x :: Float -> Picture)) r)


getSimulation :: ByteString -> IO (Either [String] Simulation)
getSimulation src = do
    r <- getValue "Simulation initial step draw"
                  "Simulation"
                  src
    return (right (\x -> (unsafeCoerce# x :: Simulation)) r)


getValue vname tname src = readLeft =<< do
    fn <- chooseFileName ".hs"
    B.writeFile fn src
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
                GHC.log_action = addErrorTo codeErrors,
                GHC.importPaths = [ "src" ]
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
                    return (Right v)
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


chooseFileName :: String -> IO String
chooseFileName sfx = do
    let chars = ['0'..'9'] ++ ['a'..'z']
        len   = length chars
    base <- replicateM 16 $ fmap (chars !!) $ randomRIO (0, len - 1)
    return ("tmp" </> (base ++ sfx))


right :: (b -> c) -> Either a b -> Either a c
right f (Right x) = Right (f x)
right f (Left  x) = Left  x


readLeft :: Either (IORef a) b -> IO (Either a b)
readLeft (Right x) = return (Right x)
readLeft (Left  x) = fmap Left (readIORef x)

