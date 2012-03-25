{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
module Main where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Time
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe
import System.Random
import Text.Templating.Heist
import Text.XmlHtml

import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as LB

import qualified Data.ByteString.Base64 as B64

import Data.Text (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LT

import qualified Data.Aeson        as A
import qualified Data.Aeson.Encode as A

import qualified Data.Vector       as V

import qualified Data.HashMap.Lazy as HM

import App
import EventStream
import CacheMap
import Source
import Serialize
import GlossAdapters

main :: IO ()
main = do
    Right heist <- loadTemplates "web" defaultHeistState
    app         <- newApp heist
    quickHttpServe $
        route [ ("draw",              draw app),
                ("anim",              anim app),
                ("sim",               sim  app),
                ("game",              game app),
                ("displayInBrowser",  displayInBrowser  app),
                ("animateInBrowser",  animateInBrowser  app),
                ("simulateInBrowser", simulateInBrowser app),
                ("gameInBrowser",     gameInBrowser     app),
                ("playStream",        playStream        app),
                ("playEvent",         playEvent         app),
                ("api/:type",         apiTop            app),
                ("api/:type/:hash",   apiWorld          app),
                ("api/session/:sid",  apiSession        app) ]
        <|> serveDirectory "web"


toFullURL :: Text -> Snap Text
toFullURL p = do
    secure <- getsRequest rqIsSecure
    host   <- getsRequest rqServerName
    port   <- getsRequest rqServerPort
    uri    <- getsRequest rqURI
    let scheme = if secure then "https" else "http"
    let defPort | secure    = 443
                | otherwise = 80
    let baseURI = fst $ T.breakOn "/api" (T.decodeUtf8 uri)
    return $ scheme
          <> "://"
          <> T.decodeUtf8 host
          <> if defPort == port then "" else ":" `mappend` T.pack (show port)
          <> baseURI
          <> p


toWorldType :: ByteString -> WorldType
toWorldType "picture"    = PictureType
toWorldType "animation"  = AnimationType
toWorldType "simulation" = SimulationType
toWorldType "game"       = GameType


fromWorldType :: WorldType -> ByteString
fromWorldType PictureType    = "picture"
fromWorldType AnimationType  = "animation"
fromWorldType SimulationType = "simulation"
fromWorldType GameType       = "game"


compileWorld :: WorldType
             -> Either ByteString ByteString
             -> App
             -> Snap (ByteString, ByteString, CompileResult)
compileWorld typ src app = liftIO $ getCompileResult (appPrograms app) typ exp src
  where exp = case typ of
            PictureType    -> "G.worldFromPicture picture :: G.CompiledWorld"
            AnimationType  -> "G.worldFromAnimation animation :: G.CompiledWorld"
            SimulationType -> "G.worldFromSimulation initial step draw :: G.CompiledWorld"
            GameType       -> "G.worldFromGame initial step event draw :: G.CompiledWorld"


apiTop :: App -> Snap ()
apiTop app     = method POST (compileNew     app)


apiWorld :: App -> Snap ()
apiWorld app   = method GET  (retrieveSource app)
             <|> method POST (startSession   app)


apiSession :: App -> Snap ()
apiSession app = method GET  (getWorldResult app)
             <|> method POST (postEvent      app)


compileNew :: App -> Snap ()
compileNew app = do
    typeId <- maybe pass return =<< getParam "type"
    src    <- (B.concat . LB.toChunks) <$> readRequestBody maxSize

    let typ = toWorldType typeId
    (dig, _, res) <- compileWorld typ (Right src) app

    sendProgInfo typ dig src res
  where maxSize = 256 * 1024


retrieveSource :: App -> Snap ()
retrieveSource app = do
    typeId <- maybe pass return =<< getParam "type"
    digest <- maybe pass return =<< getParam "hash"

    let typ      = toWorldType typeId
    let Just dig = B64.decodeLenient <$> urlDecode digest

    (_, src, res) <- compileWorld typ (Left dig) app

    sendProgInfo typ dig src res


sendProgInfo :: WorldType -> ByteString -> ByteString -> CompileResult -> Snap ()
sendProgInfo typ dig src (msgs, res) = do
    when (dig == "") $ do
        modifyResponse (setResponseCode 404)
        finishWith =<< getResponse

    url <- toFullURL $ "/api/" <> T.decodeUtf8 (fromWorldType typ) <> "/" <> encDig

    props <- case res of
        Nothing -> return [
                ("success", A.Bool False)
                ]
        Just w -> do
            g   <- liftIO newStdGen
            return [
                ("success", A.Bool True),
                ("picture", A.String $ T.decodeUtf8 $ toByteString
                          $ base64 $ fromPicture $ drawWorld $ w g),
                ("session", A.Bool $ typ /= PictureType)
                ]

    modifyResponse $ setContentType "application/json"
    writeLazyText $ LT.toLazyText $ A.fromValue $ A.Object $ HM.fromList $
        props ++ [
            ("uri",      A.String $ url),
            ("messages", A.Array $ V.fromList
                         $ map (A.String . T.pack) msgs)
            ]
  where encDig = T.decodeUtf8 $ urlEncode $ B64.encode dig


startSession :: App -> Snap ()
startSession = undefined


getWorldResult :: App -> Snap ()
getWorldResult = undefined


postEvent :: App -> Snap ()
postEvent = undefined


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
        TextNode "initial _           = BallAt 100 0\\n",
        TextNode "step t (BallAt x v) = BallAt (x+v*t) (0.99*v-x*t)\\n",
        TextNode "draw   (BallAt x v) = translate x x (circle 20)';"
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
        TextNode "var initialSource = 'import Graphics.Gloss\\n",
        TextNode "import Graphics.Gloss.Interface.Game\\n\\n",
        TextNode "initial _ = (0.0,0.0) :: Point\\n\\n",
        TextNode "event (EventMotion (x,y)) world = (x,y)\\n",
        TextNode "event _                   world = world\\n\\n",
        TextNode "step time world = world\\n\\n",
        TextNode "draw (x,y) = translate x y (circle 50)';"
        ]]


getSource :: Snap (Either ByteString ByteString)
getSource = (maybe pass (return . Right) =<< getParam "source")
    <|> (maybe pass (return . Left . B64.decodeLenient)  =<< getParam "digest")


doInBrowser :: WorldType
            -> (Text -> App -> ByteString -> World -> Snap ())
            -> App
            -> Snap ()
doInBrowser typ present app = do
    src <- getSource
    (dig, _, res) <- compileWorld typ src app
    case res of
        (errs, Nothing)  -> errors app errs
        (_,    Just res) -> do g <- liftIO newStdGen
                               present handler app dig (res g)
  where handler | typ == PictureType    = "displayInBrowser"
                | typ == AnimationType  = "animateInBrowser"
                | typ == SimulationType = "simulateInBrowser"
                | typ == GameType       = "gameInBrowser"


displayInBrowser :: App -> Snap ()
displayInBrowser  = doInBrowser PictureType displayResult


animateInBrowser :: App -> Snap ()
animateInBrowser  = doInBrowser AnimationType playResult


simulateInBrowser :: App -> Snap ()
simulateInBrowser = doInBrowser SimulationType playResult


gameInBrowser :: App -> Snap ()
gameInBrowser     = doInBrowser GameType playResult


displayResult :: Text -> App -> ByteString -> World -> Snap ()
displayResult typ app dig w = do
    let pic = drawWorld w
    Just (b, t) <- renderTemplate
        (     bindSplice "displayScript" (scrSplice pic)
            $ bindSplice "share" (shareLink typ dig)
            $ appHeist app)
        "display"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice pic = return [ Element "script" [("type", "text/javascript")] [
        TextNode "picture = '",
        TextNode $ T.decodeUtf8 $ toByteString $ base64 $ fromPicture pic,
        TextNode "';"
        ]]


playResult :: Text -> App -> ByteString -> World -> Snap ()
playResult typ app dig w = do
    t     <- liftIO getCurrentTime
    wvar  <- liftIO $ newMVar (t, 0, w)
    k     <- liftIO $ cacheNew (appSessions app) wvar
    liftIO $ keepAlive (appSessions app) k 30
    Just (b, ct) <- renderTemplate
        (     bindSplice "displayScript" (scrSplice k)
            $ bindSplice "share" (shareLink typ dig)
            $ appHeist app)
        "display"
    modifyResponse (setContentType ct)
    writeBuilder b
  where
    scrSplice k = return [ Element "script" [("type", "text/javascript")] [
        TextNode "streamURI = \'playStream?key=",
        TextNode $ T.pack $ show k,
        TextNode "\';",
        TextNode "eventURI = \'playEvent?key=",
        TextNode $ T.pack $ show k,
        TextNode "\';"
        ]]


playStream :: App -> Snap ()
playStream app = do
    k   <- fmap (read . BC.unpack) $ maybe pass return =<< getParam "key"
    var <- maybe pass return =<< liftIO (getCached (appSessions app) k)
    source   <- cullDuplicates $ modifyMVar var $ \(t0, prev, w) -> do
        keepAlive (appSessions app) k 30
        t1 <- getCurrentTime
        let interval = t1 `diffUTCTime` t0
        when (interval < targetInterval) $
            threadDelay (round (1000000 * (targetInterval - interval)))
        t1 <- getCurrentTime
        let dt = realToFrac (t1 `diffUTCTime` t0)
        let w' = stepWorld dt w
        let pic  = drawWorld w'
        return ((t1, prev, w'), pic)
    eventStreamPull (fmap pictureEvent source)
  where targetInterval = 0.05

playEvent :: App -> Snap ()
playEvent app = do
    nstr <- maybe pass return =<< getParam "n"
    case reads (BC.unpack nstr) of
        ((n,"") : _) -> mapM_ handle [0 .. n-1]
        _            -> pass
  where
    handle i = do
        typ <- maybe pass return =<< getParam (pname "type" i)
        case typ of
            "k" -> key i
            "m" -> move i
            _   -> pass

    key i = do
        k     <- toKey      =<< maybe pass return =<< getParam (pname "btn"   i)
        d     <- toKeyState =<< maybe pass return =<< getParam (pname "state" i)
        shift <- toKeyState =<< maybe pass return =<< getParam (pname "shift" i)
        alt   <- toKeyState =<< maybe pass return =<< getParam (pname "alt"   i)
        ctrl  <- toKeyState =<< maybe pass return =<< getParam (pname "ctrl"  i)
        x     <- bsToNum    =<< maybe pass return =<< getParam (pname "x"     i)
        y     <- bsToNum    =<< maybe pass return =<< getParam (pname "y"     i)
        dispatch True  $ EventKey k d (Modifiers shift ctrl alt) (x,y)

    move i = do
        x <- bsToNum =<< maybe pass return =<< getParam (pname "x" i)
        y <- bsToNum =<< maybe pass return =<< getParam (pname "y" i)
        dispatch False $ EventMotion (x,y)

    dispatch force event = do
        k   <- fmap (read . BC.unpack) $ maybe pass return =<< getParam "key"
        ts  <- fmap (read . BC.unpack) $ maybe pass return =<< getParam "ts"
        var <- maybe pass return =<< liftIO (getCached (appSessions app) k)
        liftIO $ modifyMVar var $ \ (t0, prev, w) -> do
            if force || ts >= prev
                then return ((t0, ts  , signalWorld event w), ())
                else return ((t0, prev, w                  ), ())

    pname s i = B.append s (BC.pack (show i))


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


cullDuplicates :: (Eq a, MonadIO m) => IO a -> m (IO (Maybe a))
cullDuplicates source = do
    oldVar <- liftIO (newMVar Nothing)
    return $ do
        new <- source
        old <- swapMVar oldVar (Just new)
        return $ case old of
            Nothing            -> Just new
            Just o | o == new  -> Nothing
                   | otherwise -> Just new


pictureEvent :: Maybe Picture -> ServerEvent
pictureEvent Nothing  = CommentEvent mempty
pictureEvent (Just p) = ServerEvent Nothing Nothing [ base64 $ fromPicture p ]


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


shareLink handler digest = return [ TextNode link ]
    where link = "/" `mappend` handler `mappend` "?digest="
            `mappend` (T.decodeUtf8 $ urlEncode $ B64.encode digest)
