{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveDataTypeable   #-}

module Instances where

import Blaze.ByteString.Builder
import Codec.Compression.Zlib
import Control.Monad
import Crypto.Hash.MD5
import Data.Aeson hiding (Array)
import Data.Array
import Data.Bits
import Data.Ix
import Data.List
import Data.Monoid
import Data.Typeable
import Data.Word
import Graphics.Gloss
import System.IO.Unsafe
import System.Directory

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64


deriving instance Typeable Picture

crcTable :: Array Word8 Word32
crcTable = array (0,255) [ (i, crcFor i) | i <- [0..255] ]
    where crcFor n = foldl' step (fromIntegral n) [0..7]
          step c _ | odd c     = 0xedb88320 `xor` (c `shiftR` 1)
                   | otherwise = c `shiftR` 1

crc :: LB.ByteString -> Word32
crc x = LB.foldl' step 0xffffffff x `xor` 0xffffffff
  where step c x = (crcTable ! (fromIntegral c `xor` x)) `xor` (c `shiftR` 8)


chunk :: ByteString -> LB.ByteString -> Builder
chunk nm dt = mconcat [
    fromWord32be (fromIntegral (LB.length dt)),
    fromByteString nm,
    fromLazyByteString dt,
    fromWord32be (crc dt)
    ]

ifilter :: Int -> ByteString -> LB.ByteString
ifilter w bs = LB.fromChunks (flist bs)
  where
    flist x | B.null x  = []
            | otherwise = let (s, r) = B.splitAt (4*w) x
                          in  B.singleton 0 : s : flist r


bmpToPng :: Int -> Int -> ByteString -> Builder
bmpToPng w h bs = mconcat [
    signature,
    ihdrChunk,
    idatChunk,
    iendChunk
    ]
  where
    signature = fromByteString (B.pack [ 137, 80, 78, 71, 13, 10, 26, 10 ])
    ihdrChunk = chunk "IHDR" $ toLazyByteString $ mconcat [
        fromWord32be (fromIntegral w),
        fromWord32be (fromIntegral h),
        fromWord8 8,
        fromWord8 6,
        fromWord8 0,
        fromWord8 0,
        fromWord8 0
        ]
    idatChunk = chunk "IDAT" (compress (ifilter w bs))
    iendChunk = chunk "IEND" LB.empty


cachedBmpToPng :: Int -> Int -> ByteString -> IO LB.ByteString
cachedBmpToPng w h bs = do
    let digest = BC.unpack $ B64.encode $ hash bs
    let fn     = "tmp/" ++ digest ++ ".png"
    e <- doesFileExist fn
    case e of
        True  -> LB.readFile fn
        False -> do let res = toLazyByteString (bmpToPng w h bs)
                    LB.writeFile fn res
                    return res


toBitmap :: Int -> Int -> ByteString -> Text
toBitmap w h bs = unsafePerformIO $ do
    png <- cachedBmpToPng w h bs
    let spng = B.concat (LB.toChunks png)
    return ("data:image/png;base64," `T.append` T.decodeUtf8 (B64.encode spng))


instance ToJSON Picture where
    toJSON Blank             = Null
    toJSON (Polygon path)    = object [ "t" .= ("p" :: Text), "p" .= path ]
    toJSON (Line path)       = object [ "t" .= ("l" :: Text), "p" .= path ]
    toJSON (Circle r)        = object [ "t" .= ("c" :: Text), "r" .= r ]
    toJSON (ThickCircle r w) = object [ "t" .= ("h" :: Text), "w" .= w, "r" .= r ]
    toJSON (Text str)        = object [ "t" .= ("t" :: Text), "c" .= T.pack str ]
    toJSON (Bitmap w h bmp)  = object [ "t" .= ("b" :: Text), "c" .= toBitmap w h bmp ]
    toJSON (Color c p)       = let (r,g,b,a) = rgbaOfColor c
                               in  object [ "t" .= ("z" :: Text)
                                          , "r" .= (round (255 * r) :: Int)
                                          , "g" .= (round (255 * g) :: Int)
                                          , "b" .= (round (255 * b) :: Int)
                                          , "a" .= (round (255 * a) :: Int)
                                          , "p" .= p ]
    toJSON (Translate x y p) = object [ "t" .= ("x" :: Text)
                                      , "x" .= x
                                      , "y" .= y
                                      , "p" .= p ]
    toJSON (Rotate r p)      = object [ "t" .= ("r" :: Text)
                                      , "r" .= r
                                      , "p" .= p ]
    toJSON (Scale x y p)     = object [ "t" .= ("s" :: Text)
                                      , "x" .= x
                                      , "y" .= y
                                      , "p" .= p ]
    toJSON (Pictures ps)     = toJSON ps


instance ToJSON Point where
    toJSON (x,y) = object [ "x" .= x, "y" .= y ]

