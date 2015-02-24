------------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2015 Sajith Sasidharan
-- License     :  BSD3 (see LICENSE)
--
-- Maintainer  :  sajith@nonzen.in
-- Stability   :  experimental
-- Portability :  unknown
--
-- A program that converts BMP (v3) files to XPM (v3) files.
--
-- Only 24-bit pixels are supported.  Compression or ICC color
-- profiles is not supported.
--
-- That is silly enough, and something ImageMagick can trivially do.
-- My intention however is to teach myself some Haskell concurrency.
--
------------------------------------------------------------------------------------

{--

 TODO:

  [ ] The actual image conversion.
  [ ] Use parallelsim and/or concurrency.
  [ ] Measure space usage, try pipes/conduit.
  [ ] More useful debug/error messages.
  [ ] Use exceptions instead of 'error'.
  [ ] Documentation.

--}

------------------------------------------------------------------------------------

module Main where

import           Control.Monad         (unless, when)
import           Data.Binary.Get       (Get, getWord16le, getWord32le, getWord8,
                                        runGet)
import           System.Directory      (doesFileExist, getPermissions, readable,
                                        writable)
import           System.Environment    (getArgs, getProgName)
import           System.FilePath       (replaceExtension)
import           System.IO

import           Data.Char             (chr, ord)
import           Data.Word             (Word16, Word32, Word8)

import           Data.List             (group, sort)
import           Unsafe.Coerce         (unsafeCoerce)

import qualified Data.ByteString.Char8 as BLC
import qualified Data.ByteString.Lazy  as BL

------------------------------------------------------------------------------------

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
    , imageWidth      :: Word32 -- Width of image, in pixels. u32.
    , imageHeight     :: Word32 -- Height of image, in pixels. u32.
    , colorPlanes     :: Word16 -- Number of color planes.  Always 1 for BMP files.
    , bitsPerPixel    :: Word16 -- Number of bits per pixel. legal values: 1,4,8,24.
    , compression     :: Word32 -- Compression method used.
    , bitmapSize      :: Word32 -- Size of bitmap image data, in bytes.
    , xResolution     :: Word32 -- Horizontal resolution, in pixels per meter. u32.
    , yResolution     :: Word32 -- Vertical resolution, in pixels per meter. u32.
    , colorsUsed      :: Word32 -- Number of colors in the bitmap.
    , colorsImportant :: Word32 -- Min. number of important colors.
    } deriving Show

data BmpPixel = BmpPixel {
      red   :: Word8
    , green :: Word8
    , blue  :: Word8
    -- , none  :: Word8
    } deriving Show

-- |
-- Compression types that can be present in a BMP file.
--
--   0 - no compression
--   1 - 8-bit run length encoding
--   2 - 4-bit run length encoding
--   3 - RGB bitmap with mask
--
-- Only None is supported.
--
data CompressionTypes = None | EightBitRL | FourBitRL | RGBBitmapWithMask
                      deriving (Ord, Enum, Show, Read, Eq)

-- type BmpBitmap = BL.ByteString
type BmpBitmap = [BmpPixel]

data BmpFile = BmpFile BmpFileHeader BmpInfoHeader BmpBitmap

-----------------------------------------------------------------------------

bmpFileHeaderSize :: Integer
bmpFileHeaderSize = 14

-- | BMP filetype magic
bmpFileType :: Word16
bmpFileType = toEnum $ ord 'M' * 256 + ord 'B'
-- bmpFileType = 0x4d42 -- 19778 in decimal.

-----------------------------------------------------------------------------

main :: IO ()
main = do
   args <- getArgs
   prog <- getProgName

   case args of
       [inf]       -> process inf (replaceExtension inf "xpm")
       [inf, outf] -> process inf outf
       _           -> showUsage prog

-----------------------------------------------------------------------------

showUsage :: String -> IO ()
showUsage prog = hPutStrLn stderr $
                 "Usage: " ++ prog ++ " input [output]"

-----------------------------------------------------------------------------

isFileReadable :: FilePath -> IO Bool
isFileReadable f = do
    e <- doesFileExist f
    if e then do
        p <- getPermissions f
        return $ readable p
    else return False

-----------------------------------------------------------------------------

isFileWritable :: FilePath -> IO Bool
isFileWritable f = do
    e <- doesFileExist f
    if e then do
        p <- getPermissions f
        when (writable p) $
            putStrLn$ "File " ++ show f ++ " exists. Will overwrite."
        return $ writable p
    else return True

-----------------------------------------------------------------------------

process :: FilePath -> FilePath -> IO ()
process infile outfile = do

    e1 <- isFileReadable infile
    unless e1 $ error $ "Can't read input file " ++ show infile

    e2 <- isFileWritable outfile
    unless e2 $ error $ "Can't write to output file " ++ show outfile

    withBinaryFile infile ReadMode
        (\inh -> withFile outfile WriteMode
           (\outh -> do
                 size <- hFileSize inh
                 when (size < bmpFileHeaderSize) $
                    error "Input file is too small to be a bitmap file."

                 hSetNewlineMode outh NewlineMode{ inputNL = LF,
                                                   outputNL = LF }
                 runConversion inh outh))

    -- withFile infile ReadMode
    --     (withFile outfile WriteMode . runConversion)

    putStrLn $ infile ++ " -> " ++ outfile ++ " conversion done."

-----------------------------------------------------------------------------

runConversion :: Handle -> Handle -> IO ()
runConversion bmpHandle xpmHandle = do

    putStrLn "running..."

    contents <- BL.hGetContents bmpHandle

    -- TODO: do away with too many 'from...' calls
    let bmphdr  = runGet readBmpFileHeader contents
        infoOff = fromInteger bmpFileHeaderSize
        bmpinfo = runGet readBmpInfoHeader $ BL.drop infoOff contents
        bodyOff = fromInteger bmpFileHeaderSize + fromIntegral (infoHeaderSize bmpinfo)
        bmpbody = BL.drop bodyOff contents
        pixels  = getPixels bmpinfo bmpbody
        bmpdata = BmpFile bmphdr bmpinfo pixels

    when (fileType bmphdr /= bmpFileType) $
        error "File type doesn't match: input file is not a BMP file."

    putStrLn$ "BMP header  : " ++ show bmphdr
    putStrLn$ "BMP info    : " ++ show bmpinfo
    putStrLn$ "Body length : " ++ show (BL.length bmpbody)

    putStrLn$ "Read " ++ show (length pixels) ++ " pixels ("
            ++ show (3 * length pixels) ++ " bytes)"

    let bpp = bitsPerPixel bmpinfo
    when (bpp /= 24) $
        error $ "Can't run conversion: I don't know how to handle "
                ++ show bpp ++ "-bit pixels."

    when (compression bmpinfo /= 0) $
        error "Can't run conversion: I don't know how to handle compressed bitmaps."

    writeXpmFile xpmHandle (makeXpm bmpdata)

-----------------------------------------------------------------------------

type Width  = Integer
type Height = Integer

-----------------------------------------------------------------------------

getPixels :: BmpInfoHeader -> BL.ByteString -> [BmpPixel]
getPixels hdr bs = pixels
    where
        width     = fromIntegral $ imageWidth hdr
        height    = fromIntegral $ imageHeight hdr
        offsets   = [0,3.. 3 * (width * height - 1)] :: [Integer]
        nextBs n  = BL.drop (fromIntegral n) bs
        pixels    = map (runGet readBmpPixel . nextBs) offsets

-----------------------------------------------------------------------------

type XpmData = BLC.ByteString

-----------------------------------------------------------------------------

makeXpm :: BmpFile -> XpmData
makeXpm (BmpFile _ info bitmap) = xmap
  where
    width  = fromIntegral $ imageWidth info
    height = fromIntegral $ imageHeight info
    xmap   = makeXpmBitmap height width bitmap

makeXpmBitmap :: Height -> Width -> [BmpPixel] -> XpmData
makeXpmBitmap _ _ _ = BLC.pack " " -- undefined
-- makeXpmBitmap 0 _ _ = ""
-- makeXpmBitmap h w b = makeXpmRow w b ++ makeXpmBitmap (h-1) w b

-- makeXpmRow :: (Eq a, Num a) => a -> t -> [Char]
-- makeXpmRow w b | w == 0 || w == 1 || w == 2 = "" -- this is padding
--                | otherwise = makeXpmRow (w-1) b

-----------------------------------------------------------------------------

-- xpmChrRange :: String
-- xpmChrRange = map chr [48..124]

-- This range mimics imagemagick's output.
xpmChrRange :: String
xpmChrRange = " .XoO+@#$%&*=-;:>,<1234567890" ++
              "qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ" ++
              "!~^/()_`'][{}|"

-- TODO: rewrite this mawky stuff.
xpmIndices :: Int -> [String]
xpmIndices n = take n $ oneLetters ++ twoLetters xpmChrRange
  where
    oneLetters = group xpmChrRange

    twoLetters :: String -> [String]
    twoLetters []     = group ""
    twoLetters (x:xs) = map (\c -> c ++ [x]) (group xs) ++ twoLetters xs


-----------------------------------------------------------------------------

type BmpBody   = BL.ByteString
type XpmHeader = BL.ByteString
type XpmBody   = BL.ByteString

-----------------------------------------------------------------------------

xpmCharsPerPixel :: Integer
xpmCharsPerPixel = 2

xpmNumColors :: Integer
xpmNumColors = 256

type XpmName = String

-- TODO: ALL WRONG, REDO.
xpmFormHeader :: XpmName -> BmpInfoHeader -> String
xpmFormHeader name info = -- BL.pack $
    "static char *" ++ show name ++ "[] = {\n"
    ++ show (imageWidth info) ++ " " ++ show (imageHeight info) ++ " "
    ++ show xpmNumColors ++ " " ++ show xpmCharsPerPixel ++ "\",\n"
    ++ makeXpmColorIndex

-- TODO: ALL WRONG, REDO.
makeXpmColorIndex :: String
makeXpmColorIndex = ""

-----------------------------------------------------------------------------

type ColorIndex = String
type ColorTriplet = String

colorify :: BmpPixel -> (ColorIndex, ColorTriplet)
colorify = undefined

-----------------------------------------------------------------------------

writeXpmFile :: Handle -> XpmData -> IO ()
writeXpmFile handle body = do
    putStrLn "Writing file..."
    BLC.hPut handle body

-----------------------------------------------------------------------------

-- | Read BMP file header, the first 14 bytes.
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

-- | Read BMP "info" header, proceeding the file header.
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
                         , imageWidth      = width
                         , imageHeight     = height
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

-- | Read (uncompressed) pixel information
readBmpPixel :: Get BmpPixel
readBmpPixel = do
    r <- getWord8
    g <- getWord8
    b <- getWord8
    return BmpPixel { red   = r
                    , green = g
                    , blue  = b
                    }

-----------------------------------------------------------------------------
