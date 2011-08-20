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

import EventStream
import ClientManager
import Source
import Instances


data Anim = Anim {
    startTime :: UTCTime,
    animFunc  :: Float -> Picture
    }


data App = App {
    appHeist      :: TemplateState Snap,
    appAnimations :: ClientManager Anim
    }


main :: IO ()
main = do
    Right heist <- loadTemplates "web" (emptyTemplateState "web")
    animMgr     <- newClientManager
    let app = App heist animMgr
    quickHttpServe $
        route [ ("displayInBrowser", displayInBrowser app),
                ("animateInBrowser", animateInBrowser app),
                ("animateStream",    animateStream    app) ]
        <|> serveDirectory "web"


displayInBrowser :: App -> Snap ()
displayInBrowser app = do
    src <- maybe pass return =<< getParam "source"
    res <- liftIO $ getPicture src
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
    res <- liftIO $ getAnimation src
    case res of
        Left errs -> errors app errs
        Right pic -> animate app pic


animate :: App -> (Float -> Picture) -> Snap ()
animate app f = do
    t <- liftIO getCurrentTime
    let anim = Anim t f
    k <- liftIO $ newClient (appAnimations app) anim
    Just (b, t) <- renderTemplate
        (bindSplice "animateStreamScript" (scrSplice k) (appHeist app))
        "animate"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    scrSplice k = return [ Element "script" [("type", "text/javascript")] [
        TextNode "eventURI = \'animateStream?key=",
        TextNode $ T.pack $ show k,
        TextNode "\';"
        ]]


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


animateStream :: App -> Snap ()
animateStream app = do
    k                  <- maybe pass return
                      =<< getParam "key"
    (Anim t0 f, touch) <- maybe pass return
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
