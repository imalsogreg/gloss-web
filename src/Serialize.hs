{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Serialize (fromPicture, base64) where

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
import Data.Word
import Foreign hiding (unsafePerformIO)
import GHC.Exts
import GHC.Word
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

import App


{-|
    Encodes the contents of a Builder in base64.  For now, this results in
    creating it all in memory, since we need a strict ByteString for the
    base64 encoding.
-}
base64 :: Builder -> Builder
base64 = fromByteString . B64.encode . toByteString


{-|
    Serializes a picture to a packed binary format that's intended to be fast
    to render and parse.  Each picture type has a 8-bit tag, followed by
    type-specific data.  
-}
fromPicture :: Picture -> Builder
fromPicture Blank
    = fromWord8  1
fromPicture (Polygon path)
    = fromWord8  2 `mappend` fromPath path
fromPicture (Line path)
    = fromWord8  3 `mappend` fromPath path
fromPicture (Circle r)
    = fromWord8  4 `mappend` fromFloat r
fromPicture (ThickCircle r w)
    = fromWord8  5 `mappend` fromFloat r `mappend` fromFloat w
fromPicture (Text str)
    = fromWord8  6 `mappend` fromWord32be (fromIntegral (B.length t))
      `mappend` fromByteString t
  where t = T.encodeUtf8 (T.pack str)
fromPicture (Bitmap w h bmp cache)
    = fromWord8  7
fromPicture (Color c p)
    = fromWord8  8 `mappend` fromColor c `mappend` fromPicture p
fromPicture (Translate x y p)
    = fromWord8  9 `mappend` fromFloat x `mappend` fromFloat y `mappend` fromPicture p
fromPicture (Rotate r p)
    = fromWord8 10 `mappend` fromFloat r `mappend` fromPicture p
fromPicture (Scale x y p)
    = fromWord8 11 `mappend` fromFloat x `mappend` fromFloat y `mappend` fromPicture p
fromPicture (Pictures ps)
    = fromWord8 12 `mappend` mconcat (map fromPicture ps) `mappend` fromWord8 0

fromColor c
    = let (r,g,b,a) = rgbaOfColor c
      in  fromWord8 (clamp r) `mappend` fromWord8 (clamp g) `mappend`
          fromWord8 (clamp b) `mappend` fromWord8 (clamp a)
  where
    clamp f | f < 0     = 0
            | f > 1     = 255
            | otherwise = round (255 * f)

fromPath ps = fromWord32be (fromIntegral (length ps)) `mappend` mconcat (map fromPoint ps)

fromPoint (x,y) = fromFloat x `mappend` fromFloat y


{-|
    Treat a single-precision floating-point number as a Word32.  This is
    rather hackish, but also accepted as the right way to do the job.
-}
castFloat :: Float -> Word32
castFloat f = unsafePerformIO $ alloca $ \buf -> do
    poke (castPtr buf) f
    peek buf

fromFloat :: Float -> Builder
fromFloat = fromWord32be . castFloat


{-|
    Encoding a bitmap is tricky: since we don't want to draw it pixel by
    pixel, we actually build a file in PNG format, then encode that into a
    data scheme URL.  This does mean that the actual PNG ends up base64
    encoded twice (after the entire picture is encoded), but that's the
    cleanest way to do it.
-}
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


cachedBmpToPng :: Int -> Int -> ByteString -> IO Builder
cachedBmpToPng w h bs = do
    let digest = base64FileName (hash bs)
    let fn     = "tmp/" ++ digest ++ ".png"
    e <- doesFileExist fn
    case e of
        True  -> fmap fromLazyByteString (LB.readFile fn)
        False -> do let res = bmpToPng w h bs
                    LB.writeFile fn (toLazyByteString res)
                    return res


toPng :: Int -> Int -> ByteString -> ByteString
toPng w h bs = unsafePerformIO $ fmap toByteString $ cachedBmpToPng w h bs

