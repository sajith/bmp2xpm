------------------------------------------------------------------------------------
-- |
-- Module      :  Bmp
-- Copyright   :  (C) 2015 Sajith Sasidharan
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  sajith@nonzen.in
-- Stability   :  experimental
-- Portability :  unknown
--
------------------------------------------------------------------------------------

module Bmp where

-----------------------------------------------------------------------------

import           Control.Monad               (unless, when)

import qualified Data.ByteString.Lazy        as BL

import           Data.Binary.Get             (Get, getWord16le, getWord32le,
                                              getWord8, runGet)
import           Data.Int                    (Int32)
import           Data.Word                   (Word16, Word32, Word8)
import           GHC.Generics                (Generic)

import           Data.Char                   (ord)

import           System.IO

import qualified Control.Parallel.Strategies as P

-----------------------------------------------------------------------------

-- BMP version 3.x data types.

-- The 14 byte BMP file header.
data BmpFileHeader = BmpFileHeader {
      fileType  :: Word16 -- Magic identifier. Should be "BM" (0x4d42).
    , fileSize  :: Word32 -- File size in bytes.
    , reserved1 :: Word16 -- Reserved field, always 0.
    , reserved2 :: Word16 -- Reserved field, always 0.
    , offset    :: Word32 -- Offset to image data, in bytes.
    } deriving Show

-- The 14-byte BMP file is followed by this "info" header.
data BmpInfoHeader = BmpInfoHeader {
      infoHeaderSize  :: Word32 -- Size of info header, in bytes.
    , imageWidth      :: Int32  -- Width of image, in pixels.
    , imageHeight     :: Int32  -- Height of image, in pixels.
    , colorPlanes     :: Word16 -- Number of color planes.  Always 1 for BMP files.
    , bitsPerPixel    :: Word16 -- Number of bits per pixel. legal values: 1,4,8,24.
    , compression     :: Word32 -- Compression method used.
    , bitmapSize      :: Word32 -- Size of bitmap image data, in bytes.
    , xResolution     :: Word32 -- Horizontal resolution, in pixels per meter.
    , yResolution     :: Word32 -- Vertical resolution, in pixels per meter.
    , colorsUsed      :: Word32 -- Number of colors in the bitmap.
    , colorsImportant :: Word32 -- Min. number of important colors.
    } deriving Show

-- Pixel representation for uncompressed BMP files.
data BmpPixel = BmpPixel {
      blue  :: !Word8
    , green :: !Word8
    , red   :: !Word8
    } deriving (Generic, Show)

type BmpPixelRow  = [BmpPixel]    -- Pixels are laid out in rows.
type BmpBitmap    = [BmpPixelRow] -- Bitmap data is a row of rows.

-- And voila!  We have a BMP file.
data BmpFile      = BmpFile BmpFileHeader BmpInfoHeader BmpBitmap

type BitmapName   = String
type BitmapWidth  = Integer
type BitmapRowNum = Integer

instance P.NFData BmpPixel

-----------------------------------------------------------------------------

-- BMP file header is 14 bytes.
bmpFileHeaderSize :: Integer
bmpFileHeaderSize = 14

-- BMP filetype magic (0x4d42, or 19778 in decimal)
bmpFileType :: Word16
bmpFileType = toEnum $ ord 'M' * 256 + ord 'B'

-- 'compression' field of info header can have the following values:
--
--   0 - no compression
--   1 - 8-bit run length encoding
--   2 - 4-bit run length encoding
--   3 - RGB bitmap with mask
--
-- Only uncompressed BMP files are supported.
bmpCompressionSupported :: BmpInfoHeader -> Bool
bmpCompressionSupported info = compression info == 0

-- Only 24-bit pixels are supported.
bmpColorDepthSupported :: BmpInfoHeader -> Bool
bmpColorDepthSupported info = bitsPerPixel info == 24

-----------------------------------------------------------------------------

-- Read the entire bitmap file.
getBmpBitmap :: BmpInfoHeader -> BL.ByteString -> BmpBitmap
getBmpBitmap hdr bs = pixels
    where
        width  = fromIntegral $ imageWidth hdr
        height = fromIntegral $ imageHeight hdr
        range  = if imageHeight hdr > 0
                 then reverse [0..(height-1)]
                 else [0..(height-1)]
        pixels = map (\h -> getBmpPixelRow h width bs) range

-----------------------------------------------------------------------------

-- Read a scanline of BMP pixels.
getBmpPixelRow :: BitmapRowNum -> BitmapWidth -> BL.ByteString -> BmpPixelRow
getBmpPixelRow rownum width bs = row
    where
        bs'         = BL.drop (fromIntegral (rownum*width*3)) bs
        offsets     = [0,3..(width-1)*3]
        newOffset o = BL.drop (fromIntegral o) bs'
        row         = map (runGet readBmpPixel . newOffset) offsets

-----------------------------------------------------------------------------

-- Read BMP file header, the first 14 bytes.
readBmpFileHeader :: Get BmpFileHeader
readBmpFileHeader = do
    typ  <- getWord16le
    sz   <- getWord32le
    rsv1 <- getWord16le
    rsv2 <- getWord16le
    off  <- getWord32le
    return BmpFileHeader { fileType  = typ
                         , fileSize  = sz
                         , reserved1 = rsv1
                         , reserved2 = rsv2
                         , offset    = off }

-----------------------------------------------------------------------------

-- Read BMP "info" header, proceeding the file header.
readBmpInfoHeader :: Get BmpInfoHeader
readBmpInfoHeader = do
    size   <- getWord32le
    width  <- getWord32le
    height <- getWord32le
    planes <- getWord16le
    bits   <- getWord16le
    compr  <- getWord32le
    bsize  <- getWord32le
    xres   <- getWord32le
    yres   <- getWord32le
    usedc  <- getWord32le
    mainc  <- getWord32le
    return BmpInfoHeader { infoHeaderSize  = size
                         , imageWidth      = fromIntegral width
                         , imageHeight     = fromIntegral height
                         , colorPlanes     = planes
                         , bitsPerPixel    = bits
                         , compression     = compr
                         , bitmapSize      = bsize
                         , xResolution     = xres
                         , yResolution     = yres
                         , colorsUsed      = usedc
                         , colorsImportant = mainc
                         }

-----------------------------------------------------------------------------

-- Read (uncompressed) pixel information
readBmpPixel :: Get BmpPixel
readBmpPixel = do
    b <- getWord8
    g <- getWord8
    r <- getWord8
    return BmpPixel { blue  = b
                    , green = g
                    , red   = r
                    }

-----------------------------------------------------------------------------

readBmpFile :: Handle -> IO BmpFile
readBmpFile handle = do

    checkFileSize handle

    contents <- BL.hGetContents handle

    let bmpHdr  = runGet readBmpFileHeader contents
        infoOff = fromInteger bmpFileHeaderSize
        bmpInfo = runGet readBmpInfoHeader $ BL.drop infoOff contents
        infoSz  = fromIntegral (infoHeaderSize bmpInfo)
        bodyOff = fromInteger bmpFileHeaderSize + infoSz
        bmpPix  = getBmpBitmap bmpInfo $ BL.drop bodyOff contents
        bmpData = BmpFile bmpHdr bmpInfo bmpPix

    when (fileType bmpHdr /= bmpFileType) $
        error "File type doesn't match: input file is not a BMP file."

    -- putStrLn$ "BMP header  : " ++ show bmphdr
    -- putStrLn$ "BMP info    : " ++ show bmpinfo
    -- putStrLn$ "Body length : " ++ show (BL.length bmpbody)
    -- putStrLn$ "Read " ++ show (length (concat pixels)) ++ " pixels ("
    --         ++ show (3 * length (concat pixels)) ++ " bytes)"

    unless (bmpColorDepthSupported bmpInfo) $
        error $ "Can't run conversion: I don't know how to handle "
                ++ show (bitsPerPixel bmpInfo) ++ "-bit pixels."

    unless (bmpCompressionSupported bmpInfo) $
        error $ "Can't run conversion: "
                ++ "I don't know how to handle compressed bitmaps."

    return bmpData

-----------------------------------------------------------------------------

checkFileSize :: Handle -> IO ()
checkFileSize inh = do
    size <- hFileSize inh
    when (size < bmpFileHeaderSize) $
        error "Input file is too small to be a bitmap file."

-----------------------------------------------------------------------------

