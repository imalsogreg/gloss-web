{-# LANGUAGE OverloadedStrings #-}

{-|
    A Snap adapter to the HTML5 Server-Sent Events API.  Push-mode and
    pull-mode interfaces are both available.
-}
module EventStream (
    ServerEvent(..),
    eventStreamPull,
    eventStreamReliable,
    eventStreamUnreliable
    ) where

import Blaze.ByteString.Builder
import Control.Monad.Trans
import Control.Concurrent
import Data.Monoid
import Data.Enumerator.List (generateM)
import Snap.Types

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-|
    Type representing a communication over an event stream.  This can be an
    actual event, a comment, a modification to the retry timer, or a special
    "close" event indicating the server should close the connection.
-}
data ServerEvent
    = ServerEvent {
        eventName :: Maybe Text,
        eventId   :: Maybe Text,
        eventData :: Text
        }
    | CommentEvent {
        eventComment :: Text
        }
    | RetryEvent {
        eventRetry :: Int
        }
    | CloseEvent


{-|
    Wraps the text as a labeled field of an event stream.
-}
field l t = l `T.append` t `T.append` "\n"


{-|
    Appends a buffer flush to the end of a Builder.
-}
flushAfter b = b `mappend` flush


{-|
    Converts a 'ServerEvent' to its wire representation as specified by the
    @text/event-stream@ content type.
-}
eventToBuilder :: ServerEvent -> Maybe Builder
eventToBuilder (CloseEvent)        = Nothing
eventToBuilder (CommentEvent txt)  = Just $ flushAfter $ fromByteString $
    T.encodeUtf8 (field ":" txt)
eventToBuilder (RetryEvent   n)    = Just $ flushAfter $ fromByteString $
    T.encodeUtf8 (field "retry:" (T.pack (show n)))
eventToBuilder (ServerEvent n i d) = Just $ flushAfter $ fromByteString $
    T.encodeUtf8 $ T.concat $ name n $ evid i $
    map (field "data:") (T.lines d) ++ ["\n"]
  where
    name Nothing  = id
    name (Just n) = (field "event:" n :)
    evid Nothing  = id
    evid (Just i) = (field "id:"   i :)


{-|
    Sets up this request to act as an event stream, obtaining its events from
    polling the given IO action.
-}
eventStreamPull       :: IO ServerEvent -> Snap ()
eventStreamPull source = do
    modifyResponse (setContentType "text/event-stream")
    timeout <- getTimeoutAction
    modifyResponse $ setResponseBody $
        generateM (timeout 10 >> fmap eventToBuilder source)


{-|
    Sets up this request to act as an event stream, returning an action to send
    events along the stream.
-}
eventStreamPush   :: Snap (ServerEvent -> IO ())
eventStreamPush = do
    chan <- liftIO newChan
    eventStreamPull (readChan chan)
    return (writeChan chan)

