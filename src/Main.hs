{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Encode
import Data.IORef
import Data.Time
import Graphics.Gloss
import Graphics.Gloss.Interface.Game
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
import Serialize
import GlossAdapters


main :: IO ()
main = do
    Right heist <- loadTemplates "web" (emptyTemplateState "web")
    app         <- newApp heist
    quickHttpServe $
        route [ ("draw",              draw app),
                ("anim",              anim app),
                ("sim",               sim  app),
                ("game",              game app),
                ("displayInBrowser",  displayInBrowser  app),
                ("animateInBrowser",  animateInBrowser  app),
                ("animateStream",     animateStream     app),
                ("simulateInBrowser", simulateInBrowser app),
                ("simulateStream",    simulateStream    app),
                ("gameInBrowser",     gameInBrowser app),
                ("gameStream",        gameStream    app),
                ("gameEvent",         gameEvent     app) ]
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


game :: App -> Snap ()
game app = do
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
        Element "code" [] [TextNode "event"],
        TextNode ", ",
        Element "code" [] [TextNode "step"],
        TextNode ", and ",
        Element "code" [] [TextNode "draw"],
        TextNode " describing your game's initial state, event handler, step function, and appearance."
        ]
    actionSplice = return [ TextNode "gameInBrowser" ]
    defaultsSplice = return [ Element "script" [("type", "text/javascript")] [
        TextNode "var sourceCookie = 'gameSource';",
        TextNode "var initialSource = 'import Graphics.Gloss\\n\\n",
        TextNode "initial = undefined\\n",
        TextNode "event e = undefined\\n",
        TextNode "step t g = undefined\\n",
        TextNode "draw g = undefined';"
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
        TextNode "picture = '",
        TextNode $ T.decodeUtf8 $ toByteString $ base64 $ fromPicture pic,
        TextNode "';"
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
        (bindSplice "displayScript" (scrSplice k) (appHeist app))
        "display"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice k = return [ Element "script" [("type", "text/javascript")] [
        TextNode "streamURI = \'animateStream?key=",
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
        return $ ServerEvent Nothing Nothing [ base64 $ fromPicture $ f t ]
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
        (bindSplice "displayScript" (scrSplice k) (appHeist app))
        "display"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice k = return [ Element "script" [("type", "text/javascript")] [
        TextNode "streamURI = \'simulateStream?key=",
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
        return ((t1, sim'), ServerEvent Nothing Nothing [ base64 $ fromPicture pic ])
  where
    targetInterval = 0.1


gameInBrowser :: App -> Snap ()
gameInBrowser app = do
    src <- maybe pass return =<< getParam "source"
    res <- liftIO $ getGame app src
    case res of
        Left errs -> errors app errs
        Right pic -> runGame app pic


runGame :: App -> Game -> Snap ()
runGame app game = do
    t     <- liftIO getCurrentTime
    gvar  <- liftIO $ newMVar (t, game)
    k     <- liftIO $ newClient (appGames app) gvar
    Just (b, t) <- renderTemplate
        (bindSplice "displayScript" (scrSplice k) (appHeist app))
        "display"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice k = return [ Element "script" [("type", "text/javascript")] [
        TextNode "streamURI = \'gameStream?key=",
        TextNode $ T.pack $ show k,
        TextNode "\';",
        TextNode "eventURI = \'gameEvent?key=",
        TextNode $ T.pack $ show k,
        TextNode "\';"
        ]]


gameStream :: App -> Snap ()
gameStream app = do
    k                  <- maybe pass return
                      =<< getParam "key"
    (var, touch)       <- maybe pass return
                      =<< liftIO (getClient (appGames app) (read (BC.unpack k)))
    eventStreamPull $ modifyMVar var $ \(t0, game) -> do
        touch
        t1 <- getCurrentTime
        let interval = t1 `diffUTCTime` t0
        when (interval < targetInterval) $
            threadDelay $ round $ 1000000 * (targetInterval - interval)
        t1 <- getCurrentTime
        let t = realToFrac (t1 `diffUTCTime` t0)
        let game' = advanceGame t game
        let pic  = gameToPicture game'
        return ((t1, game'), ServerEvent Nothing Nothing [ base64 $ fromPicture pic ])
  where
    targetInterval = 0.1


gameEvent :: App -> Snap ()
gameEvent app = do
    liftIO . print . BC.unpack =<< fmap rqQueryString getRequest
    typ <- maybe pass return =<< getParam "type"
    case typ of
        "k" -> key
        "m" -> move
        _   -> pass
  where
    key  = do
        k     <- toKey      =<< maybe pass return =<< getParam "btn"
        d     <- toKeyState =<< maybe pass return =<< getParam "state"
        shift <- toKeyState =<< maybe pass return =<< getParam "shift"
        alt   <- toKeyState =<< maybe pass return =<< getParam "alt"
        ctrl  <- toKeyState =<< maybe pass return =<< getParam "ctrl"
        x     <- bsToNum    =<< maybe pass return =<< getParam "x"
        y     <- bsToNum    =<< maybe pass return =<< getParam "y"
        dispatch $ EventKey k d (Modifiers shift ctrl alt) (x,y)

    move = do
        x <- bsToNum =<< maybe pass return =<< getParam "x"
        y <- bsToNum =<< maybe pass return =<< getParam "y"
        dispatch $ EventMotion (x,y)

    dispatch event = do
        k                  <- maybe pass return
                          =<< getParam "key"
        (var, touch)       <- maybe pass return
                          =<< liftIO (getClient (appGames app) (read (BC.unpack k)))
        liftIO $ modifyMVar var $
            \ (t0, game) -> return ((t0, signalGame event game), ())


bsToNum :: ByteString -> Snap Float
bsToNum bs = case reads (BC.unpack bs) of
    [(f,"")] -> return f
    _        -> pass


toKeyState :: ByteString -> Snap KeyState
toKeyState "1" = return Down
toKeyState "0" = return Up
toKeyState _   = pass


toKey :: ByteString -> Snap Key
toKey bs = case bs of
    -- Special keys and mouse events; always > 1 character
    "F1"                 -> return (SpecialKey KeyF1)
    "F2"                 -> return (SpecialKey KeyF2)
    "F3"                 -> return (SpecialKey KeyF3)
    "F4"                 -> return (SpecialKey KeyF4)
    "F5"                 -> return (SpecialKey KeyF5)
    "F6"                 -> return (SpecialKey KeyF6)
    "F7"                 -> return (SpecialKey KeyF7)
    "F8"                 -> return (SpecialKey KeyF8)
    "F9"                 -> return (SpecialKey KeyF9)
    "F10"                -> return (SpecialKey KeyF10)
    "F11"                -> return (SpecialKey KeyF11)
    "F12"                -> return (SpecialKey KeyF12)
    "Left"               -> return (SpecialKey KeyLeft)
    "Up"                 -> return (SpecialKey KeyUp)
    "Right"              -> return (SpecialKey KeyRight)
    "Down"               -> return (SpecialKey KeyDown)
    "PageUp"             -> return (SpecialKey KeyPageUp)
    "PageDown"           -> return (SpecialKey KeyPageDown)
    "Home"               -> return (SpecialKey KeyHome)
    "End"                -> return (SpecialKey KeyEnd)
    "Insert"             -> return (SpecialKey KeyInsert)
    "Del"                -> return (SpecialKey KeyDelete)
    "NumLock"            -> return (SpecialKey KeyNumLock)
    "lbtn"               -> return (MouseButton LeftButton)
    "rbtn"               -> return (MouseButton RightButton)
    "mbtn"               -> return (MouseButton MiddleButton)
    "mwup"               -> return (MouseButton WheelUp)
    "mwdn"               -> return (MouseButton WheelDown)
    _ | B.length bs == 1 -> return (Char (BC.head bs))
      | otherwise        -> pass


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

