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

import           System.Directory      (doesFileExist, getPermissions, readable,
                                        writable)
import           System.Environment    (getArgs, getProgName)
import           System.FilePath       (replaceExtension, takeBaseName)
import           System.IO

import           Data.Binary.Get       (Get, getWord16le, getWord32le, getWord8,
                                        runGet)
import           Data.Char             (ord)
import           Data.List             (group, nub, sort)
import qualified Data.Map              as M
import           Data.Word             (Word16, Word32, Word8)

import           Text.Printf           (printf)

import qualified Data.ByteString.Char8 as BLC
import qualified Data.ByteString.Lazy  as BL

------------------------------------------------------------------------------------

-- BMP version 3.x data types.

-- | The 14 byte BMP file header.
data BmpFileHeader = BmpFileHeader {
      fileType  :: Word16 -- Magic identifier. Should be "BM" (0x4d42).
    , fileSize  :: Word32 -- File size in bytes.
    , reserved1 :: Word16 -- Reserved field, always 0.
    , reserved2 :: Word16 -- Reserved field, always 0.
    , offset    :: Word32 -- Offset to image data, in bytes.
    } deriving Show

-- | The 14-byte BMP file is followed by this "info" header.
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

-- | Pixel representation for uncompressed BMP files.
data BmpPixel = BmpPixel {
      red   :: Word8
    , green :: Word8
    , blue  :: Word8
    } deriving (Show, Ord)

-- | Need this to 'nub' the pixel array.
instance Eq BmpPixel where
    (==) (BmpPixel r g b) (BmpPixel x y z) = r == x && g == y && b == z
    (/=) a b = not $ (==) a b

-- | Pixels are laid out in rows.
type BmpRow    = [BmpPixel]

-- | Bitmap data is a row of rows.
type BmpBitmap = [BmpRow]

-- | And voila!  We have a BMP file.
data BmpFile = BmpFile BmpFileHeader BmpInfoHeader BmpBitmap

-----------------------------------------------------------------------------

-- | BMP file header is 14 bytes.
bmpFileHeaderSize :: Integer
bmpFileHeaderSize = 14

-- | BMP filetype magic (0x4d42, or 19778 in decimal)
bmpFileType :: Word16
bmpFileType = toEnum $ ord 'M' * 256 + ord 'B'

-- | 'compression' field of info header can have the following values:
--
--   0 - no compression
--   1 - 8-bit run length encoding
--   2 - 4-bit run length encoding
--   3 - RGB bitmap with mask
--
-- Only uncompressed BMP files are supported.
bmpCompressionSupported :: Word32
bmpCompressionSupported = 0

-- | Only 24-bit pixels are supported.
bmpColorDepthSupported :: Word16
bmpColorDepthSupported = 24

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
                 prog ++ " converts BMP files to XMP files.\n\n" ++
                 "Usage: " ++ prog ++ " input [output]\n\n" ++
                 "input should be a Windows bitmap file."

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

    let name = takeBaseName infile

    withBinaryFile infile ReadMode
        (withFile outfile WriteMode . runConversion name)

    putStrLn $ infile ++ " -> " ++ outfile ++ " conversion done."

-----------------------------------------------------------------------------

type Name = String

-----------------------------------------------------------------------------

checkFileSize :: Handle -> IO ()
checkFileSize inh = do
    size <- hFileSize inh
    when (size < bmpFileHeaderSize) $
        error "Input file is too small to be a bitmap file."

-----------------------------------------------------------------------------

-- TODO: temporary debugging function, remove this.
showRows :: BmpFile -> IO ()
showRows (BmpFile _ _ pixels) = mapM_ showLength pixels
  where
    showLength x = putStrLn$ "length: " ++ show (length x)

-----------------------------------------------------------------------------

runConversion :: Name -> Handle -> Handle -> IO ()
runConversion name bmpHandle xpmHandle = do

    putStrLn "running..."

    checkFileSize bmpHandle

    contents <- BL.hGetContents bmpHandle

    -- TODO: do away with too many 'from...' calls
    let bmphdr  = runGet readBmpFileHeader contents
        infoOff = fromInteger bmpFileHeaderSize
        bmpinfo = runGet readBmpInfoHeader $ BL.drop infoOff contents
        bodyOff = fromInteger bmpFileHeaderSize + fromIntegral (infoHeaderSize bmpinfo)
        bmpbody = BL.drop bodyOff contents
        pixels  = getBmpBitmap bmpinfo bmpbody
        bmpdata = BmpFile bmphdr bmpinfo pixels

    when (fileType bmphdr /= bmpFileType) $
        error "File type doesn't match: input file is not a BMP file."

    putStrLn$ "BMP header  : " ++ show bmphdr
    putStrLn$ "BMP info    : " ++ show bmpinfo
    putStrLn$ "Body length : " ++ show (BL.length bmpbody)

    putStrLn$ "Read " ++ show (length pixels) ++ " pixels ("
            ++ show (3 * length pixels) ++ " bytes)"

    showRows bmpdata

    when (bitsPerPixel bmpinfo /= bmpColorDepthSupported) $
        error $ "Can't run conversion: I don't know how to handle "
                ++ show (bitsPerPixel bmpinfo) ++ "-bit pixels."

    when (compression bmpinfo /= bmpCompressionSupported) $
        error "Can't run conversion: I don't know how to handle compressed bitmaps."

    let xpmdata = makeXpm name bmpdata
    putStrLn$ "Xpm conversion result size: " ++ show (BLC.length xpmdata)

    writeXpmFile xpmHandle xpmdata

-----------------------------------------------------------------------------

type Width  = Integer
type Height = Integer
type RowNum = Integer

-----------------------------------------------------------------------------

getBmpBitmap :: BmpInfoHeader -> BL.ByteString -> BmpBitmap
getBmpBitmap hdr bs = pixels
  where
    width     = fromIntegral $ imageWidth hdr
    height    = fromIntegral $ imageHeight hdr
    pixels    = map (\h -> getBmpRow h width bs) [0..(height-1)]

getBmpRow :: RowNum -> Width -> BL.ByteString -> BmpRow
getBmpRow rownum width bs = row
  where
    bs'         = BL.drop (fromIntegral (rownum*width)) bs
    offsets     = [0,3..(width-1)*3]
    newOffset o = BL.drop (fromIntegral o) bs'
    row         = map (runGet readBmpPixel . newOffset) offsets

-----------------------------------------------------------------------------

type XpmColor  = BLC.ByteString -- format: "xx c #xxxxxx"
type XpmColors = BLC.ByteString
type XpmBitmap = BLC.ByteString
type XpmPixel  = String -- BLC.ByteString
type XpmRow    = BLC.ByteString
type XpmData   = BLC.ByteString -- = XpmHeader + XpmColors + [XpmRow] + XpmTail

-----------------------------------------------------------------------------

-- toXpmColor :: BmpPixel -> XpmPixel
-- toXpmColor (BmpPixel r g b) = BLC.pack $ printf "#%02x%02x%02x" r g b

-----------------------------------------------------------------------------

quote :: BLC.ByteString -> BLC.ByteString
quote bs = BLC.snoc (BLC.cons dq bs) dq
           where dq = '\"'

-----------------------------------------------------------------------------

makeXpm :: Name -> BmpFile -> XpmData
makeXpm name (BmpFile _ info pixels) = xpmdata -- TODO: do this correctly
  where
    header       = xpmFormHeader name info
    -- xmap      = BLC.intercalate (BLC.pack ",\n")
    --          $ map xpmMakeBitmap pixels
    (cmap, xmap) = xpmMakeBitmap pixels
    xpmColors    = xpmMakeColorMap cmap
    xpmheader    = BLC.append header xpmColors
    xpmbody      = BLC.append xpmheader xmap
    xpmdata      = BLC.append xpmbody (BLC.pack "\n};")

-----------------------------------------------------------------------------

xpmMakeColorMap :: XpmColorMap -> XpmColors
xpmMakeColorMap cmap = rows
  where
    -- assocs = M.toList cmap
    assocs = M.assocs cmap
    rows   = BLC.concat $ sort $ map translateColor assocs

translateColor :: (BmpPixel, XpmIndex) -> XpmColor
translateColor (b, c) = BLC.pack $ printf "\"%s c %s\",\n" c (toXpmColor b)

-----------------------------------------------------------------------------

xpmMakeBitmap :: BmpBitmap -> (XpmColorMap, XpmBitmap)
xpmMakeBitmap bmprows = (cmap, xmap)
  where
    cmap    = makeColorMap $ nub (concat bmprows)
    xpmrows = map (quote . translateRow cmap) bmprows
    xmap    = BLC.intercalate (BLC.pack ",\n") xpmrows

translateRow :: XpmColorMap -> BmpRow -> XpmRow
translateRow cmap row = BLC.pack $ concatMap (translatePixel cmap) row

-- TODO: Remove debug stuff
-- translateRow :: XpmColorMap -> BmpRow -> XpmRow
-- translateRow cmap row = BLC.pack $ (show $ length row) ++ " " ++ row'
--   where row' = concatMap (translatePixel cmap) row

translatePixel :: XpmColorMap -> BmpPixel -> XpmPixel
translatePixel m p = case M.lookup p m of
               Just c  -> c ++ " "
               Nothing -> ""

-----------------------------------------------------------------------------

-- type XpmColor    = String
type XpmIndex    = String
type XpmColorMap = M.Map BmpPixel XpmIndex

-----------------------------------------------------------------------------

-- toXpmColor :: BmpPixel -> XpmColor
-- toXpmColor (BmpPixel r g b) = BLC.pack $ printf "#%02x%02x%02x" r g b

-- TODO: this translation is incorrect, fix.
toXpmColor :: BmpPixel -> String
toXpmColor (BmpPixel r g b) = printf "#%02X%02X%02X" r g b

-----------------------------------------------------------------------------

makeColorMap :: [BmpPixel] -> XpmColorMap
makeColorMap pixels = M.fromList $ zip pixels xpmIndices

-----------------------------------------------------------------------------

-- xpmChrRange :: String
-- xpmChrRange = map chr [48..124]

-- This range mimics imagemagick's output.
xpmChrRange :: String
xpmChrRange = " .XoO+@#$%&*=-;:>,<1234567890" ++
              "qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ" ++
              "!~^/()_`'][{}|"

-- TODO: rewrite this mawky stuff.
-- xpmIndices :: Int -> [String]
-- xpmIndices n = take n $ oneLetters ++ twoLetters xpmChrRange
--   where
--     oneLetters = group xpmChrRange

--     twoLetters :: String -> [String]
--     twoLetters []     = group ""
--     twoLetters (x:xs) = map (\c -> c ++ [x]) (group xs) ++ twoLetters xs

xpmIndices :: [XpmIndex]
xpmIndices = oneLetters ++ twoLetters xpmChrRange
  where
    oneLetters = group xpmChrRange
    -- TODO: rewrite this.
    twoLetters :: String -> [String]
    twoLetters []     = group ""
    twoLetters (x:xs) = map (\c -> c ++ [x]) (group xs) ++ twoLetters xs

-----------------------------------------------------------------------------

type BmpBody   = BLC.ByteString
type XpmHeader = BLC.ByteString
type XpmBody   = BLC.ByteString

-----------------------------------------------------------------------------

xpmCharsPerPixel :: Integer
xpmCharsPerPixel = 2

xpmNumColors :: Integer
xpmNumColors = 256

xpmFormHeader :: Name -> BmpInfoHeader -> XpmHeader
xpmFormHeader name info = BLC.pack $
    "/* XPM */\n"
    ++ "static char *" ++ name ++ "[] = {\n\""
    ++ "/* columns rows colors chars-per-pixel */\n"
    ++ show (imageWidth info) ++ " " ++ show (imageHeight info) ++ " "
    ++ show xpmNumColors ++ " " ++ show xpmCharsPerPixel ++ "\",\n"

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
