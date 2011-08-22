{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.IORef
import Data.Time
import Graphics.Gloss
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import Text.Templating.Heist
import Text.XmlHtml

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import App
import EventStream
import ClientManager
import Source
import Instances
import GlossAdapters


main :: IO ()
main = do
    Right heist <- loadTemplates "web" (emptyTemplateState "web")
    app         <- newApp heist
    quickHttpServe $
        route [ ("draw",              draw app),
                ("anim",              anim app),
                ("sim",               sim app),
                ("displayInBrowser",  displayInBrowser  app),
                ("animateInBrowser",  animateInBrowser  app),
                ("animateStream",     animateStream     app),
                ("simulateInBrowser", simulateInBrowser app),
                ("simulateStream",    simulateStream    app) ]
        <|> serveDirectory "web"


draw :: App -> Snap ()
draw app = do
    Just (b,t) <- renderTemplate
        (addSplices (appHeist app))
        "editor"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    addSplices = bindSplices [
        ("intro", introSplice),
        ("action", actionSplice),
        ("defaults", defaultsSplice)
        ]
    introSplice = return [
        TextNode "Define a variable called ",
        Element "code" [] [TextNode "picture"],
        TextNode " describing your picture."
        ]
    actionSplice = return [ TextNode "displayInBrowser" ]
    defaultsSplice = return [ Element "script" [("type", "text/javascript")] [
        TextNode "var sourceCookie = 'displaySource';",
        TextNode "var initialSource = 'import Graphics.Gloss\\n\\n",
        TextNode "picture = circle 80';"
        ]]


anim :: App -> Snap ()
anim app = do
    Just (b,t) <- renderTemplate
        (addSplices (appHeist app))
        "editor"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    addSplices = bindSplices [
        ("intro", introSplice),
        ("action", actionSplice),
        ("defaults", defaultsSplice)
        ]
    introSplice = return [
        TextNode "Define a variable called ",
        Element "code" [] [TextNode "animation"],
        TextNode " describing your animation."
        ]
    actionSplice = return [ TextNode "animateInBrowser" ]
    defaultsSplice = return [ Element "script" [("type", "text/javascript")] [
        TextNode "var sourceCookie = 'animateSource';",
        TextNode "var initialSource = 'import Graphics.Gloss\\n\\n",
        TextNode "animation t = rotate (60*t) (rectangleSolid 80 200)';"
        ]]


sim :: App -> Snap ()
sim app = do
    Just (b,t) <- renderTemplate
        (addSplices (appHeist app))
        "editor"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    addSplices = bindSplices [
        ("intro", introSplice),
        ("action", actionSplice),
        ("defaults", defaultsSplice)
        ]
    introSplice = return [
        TextNode "Define variables called ",
        Element "code" [] [TextNode "initial"],
        TextNode ", ",
        Element "code" [] [TextNode "step"],
        TextNode ", and ",
        Element "code" [] [TextNode "draw"],
        TextNode " describing your simulation's initial state, step function, and appearance."
        ]
    actionSplice = return [ TextNode "simulateInBrowser" ]
    defaultsSplice = return [ Element "script" [("type", "text/javascript")] [
        TextNode "var sourceCookie = 'simulateSource';",
        TextNode "var initialSource = 'import Graphics.Gloss\\n\\n",
        TextNode "data BallState = BallAt Float Float\\n",
        TextNode "initial = BallAt 100 0\\n",
        TextNode "step _ t (BallAt x v) = BallAt (x+v*t) (0.99*v-x*t)\\n",
        TextNode "draw     (BallAt x v) = translate x x (circle 20)';"
        ]]


displayInBrowser :: App -> Snap ()
displayInBrowser app = do
    src <- maybe pass return =<< getParam "source"
    res <- liftIO $ getPicture app src
    case res of
        Left errs -> errors app errs
        Right pic -> display app pic


display :: App -> Picture -> Snap ()
display app pic = do
    Just (b, t) <- renderTemplate
        (bindSplice "displayScript" (scrSplice pic) (appHeist app))
        "display"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice pic = return [ Element "script" [("type", "text/javascript")] [
        TextNode "picture = ",
        TextNode $ T.decodeUtf8 $ B.concat $ LB.toChunks $ encode pic,
        TextNode ";"
        ]]


animateInBrowser :: App -> Snap ()
animateInBrowser app = do
    src <- maybe pass return =<< getParam "source"
    res <- liftIO $ getAnimation app src
    case res of
        Left errs -> errors app errs
        Right pic -> animate app pic


animate :: App -> (Float -> Picture) -> Snap ()
animate app f = do
    t <- liftIO getCurrentTime
    let anim = (t, f)
    k <- liftIO $ newClient (appAnimations app) anim
    Just (b, t) <- renderTemplate
        (bindSplice "streamScript" (scrSplice k) (appHeist app))
        "animate"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice k = return [ Element "script" [("type", "text/javascript")] [
        TextNode "eventURI = \'animateStream?key=",
        TextNode $ T.pack $ show k,
        TextNode "\';"
        ]]


animateStream :: App -> Snap ()
animateStream app = do
    k                  <- maybe pass return
                      =<< getParam "key"
    ((t0, f), touch)  <- maybe pass return
                      =<< liftIO (getClient (appAnimations app) (read (BC.unpack k)))
    tv                 <- liftIO (newIORef =<< getCurrentTime)
    eventStreamPull $ do
        touch
        t1 <- getCurrentTime
        t' <- readIORef tv
        let interval = t1 `diffUTCTime` t'
        when (interval < targetInterval) $
            threadDelay $ round $ 1000000 * (targetInterval - interval)
        t1 <- getCurrentTime
        writeIORef tv t1
        let t = realToFrac (t1 `diffUTCTime` t0)
        return $ ServerEvent Nothing Nothing $
            T.decodeUtf8 $ B.concat $ LB.toChunks $ encode (f t)
  where
    targetInterval = 0.1


simulateInBrowser :: App -> Snap ()
simulateInBrowser app = do
    src <- maybe pass return =<< getParam "source"
    res <- liftIO $ getSimulation app src
    case res of
        Left errs -> errors app errs
        Right pic -> simulate app pic


simulate :: App -> Simulation -> Snap ()
simulate app sim = do
    t     <- liftIO getCurrentTime
    simul <- liftIO $ newMVar (t, sim)
    k     <- liftIO $ newClient (appSimulations app) simul
    Just (b, t) <- renderTemplate
        (bindSplice "streamScript" (scrSplice k) (appHeist app))
        "animate"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice k = return [ Element "script" [("type", "text/javascript")] [
        TextNode "eventURI = \'simulateStream?key=",
        TextNode $ T.pack $ show k,
        TextNode "\';"
        ]]


simulateStream :: App -> Snap ()
simulateStream app = do
    k                  <- maybe pass return
                      =<< getParam "key"
    (var, touch)       <- maybe pass return
                      =<< liftIO (getClient (appSimulations app) (read (BC.unpack k)))
    eventStreamPull $ modifyMVar var $ \(t0, sim) -> do
        touch
        t1 <- getCurrentTime
        let interval = t1 `diffUTCTime` t0
        when (interval < targetInterval) $
            threadDelay $ round $ 1000000 * (targetInterval - interval)
        t1 <- getCurrentTime
        let t = realToFrac (t1 `diffUTCTime` t0)
        let sim' = advanceSimulation t sim
        let pic  = simulationToPicture sim'
        return ((t1, sim'),
            ServerEvent Nothing Nothing $
                T.decodeUtf8 $ B.concat $ LB.toChunks $ encode pic)
  where
    targetInterval = 0.1


errors :: App -> [String] -> Snap ()
errors app errs = do
    Just (b, t) <- renderTemplate
        (bindSplice "showErrors" (errSplice errs) (appHeist app))
        "errors"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    errSplice errs = return [Element "ul" []
        (map (\s -> Element "li" [] [
                Element "pre" [] [TextNode (T.pack s) ]]) errs)]

