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
translateColor (b, c) = BLC.pack $ printf "\"%2v c %7v\",\n" c (toXpmColor b)

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
               Just c  -> printf "%2v" c -- c ++ " "
               Nothing -> "##"

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

-- TODO: this makes an incorrect map, fix.
makeColorMap :: [BmpPixel] -> XpmColorMap
makeColorMap pixels = M.fromList $
                      (BmpPixel{red=255,blue=255,green=255}, "##") :
                      zip pixels xpmIndices

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
xpmIndices = oneLetters ++ filter (/= "##") (twoLetters xpmChrRange)
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
    ++ "static char *" ++ name ++ "[] = {\n"
    ++ "/* columns rows colors chars-per-pixel */\n\""
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

-- Using a static color palette here, to aid the translation from
-- RGB888 to 8-bit colors.  A better alternative would be to build a
-- color palette from a histogram of the source image's color space,
-- since that might give us a more faithful translation.  But we're
-- lazy.

type XpmPaletteColor = Integer

-- This is our 216-color palette.  (Why 216 colors? 6 choices each of
-- red, green blue gives us 6*6*6 = 216 colors.)
xpmPalette :: [XpmPaletteColor]
xpmPalette =
    [
      0x000000, 0x000033, 0x000066, 0x000099, 0x0000cc, 0x0000ff
    , 0x003300, 0x003333, 0x003366, 0x003399, 0x0033cc, 0x0033ff
    , 0x006600, 0x006633, 0x006666, 0x006699, 0x0066cc, 0x0066ff
    , 0x009900, 0x009933, 0x009966, 0x009999, 0x0099cc, 0x0099ff
    , 0x00cc00, 0x00cc33, 0x00cc66, 0x00cc99, 0x00cccc, 0x00ccff
    , 0x00ff00, 0x00ff33, 0x00ff66, 0x00ff99, 0x00ffcc, 0x00ffff

    , 0x330000, 0x330033, 0x330066, 0x330099, 0x3300cc, 0x3300ff
    , 0x333300, 0x333333, 0x333366, 0x333399, 0x3333cc, 0x3333ff
    , 0x336600, 0x336633, 0x336666, 0x336699, 0x3366cc, 0x3366ff
    , 0x339900, 0x339933, 0x339966, 0x339999, 0x3399cc, 0x3399ff
    , 0x33cc00, 0x33cc33, 0x33cc66, 0x33cc99, 0x33cccc, 0x33ccff
    , 0x33ff00, 0x33ff33, 0x33ff66, 0x33ff99, 0x33ffcc, 0x33ffff

    , 0x660000, 0x660033, 0x660066, 0x660099, 0x6600cc, 0x6600ff
    , 0x663300, 0x663333, 0x663366, 0x663399, 0x6633cc, 0x6633ff
    , 0x666600, 0x666633, 0x666666, 0x666699, 0x6666cc, 0x6666ff
    , 0x669900, 0x669933, 0x669966, 0x669999, 0x6699cc, 0x6699ff
    , 0x66cc00, 0x66cc33, 0x66cc66, 0x66cc99, 0x66cccc, 0x66ccff
    , 0x66ff00, 0x66ff33, 0x66ff66, 0x66ff99, 0x66ffcc, 0x66ffff

    , 0x990000, 0x990033, 0x990066, 0x990099, 0x9900cc, 0x9900ff
    , 0x993300, 0x993333, 0x993366, 0x993399, 0x9933cc, 0x9933ff
    , 0x996600, 0x996633, 0x996666, 0x996699, 0x9966cc, 0x9966ff
    , 0x999900, 0x999933, 0x999966, 0x999999, 0x9999cc, 0x9999ff
    , 0x99cc00, 0x99cc33, 0x99cc66, 0x99cc99, 0x99cccc, 0x99ccff
    , 0x99ff00, 0x99ff33, 0x99ff66, 0x99ff99, 0x99ffcc, 0x99ffff

    , 0xcc0000, 0xcc0033, 0xcc0066, 0xcc0099, 0xcc00cc, 0xcc00ff
    , 0xcc3300, 0xcc3333, 0xcc3366, 0xcc3399, 0xcc33cc, 0xcc33ff
    , 0xcc6600, 0xcc6633, 0xcc6666, 0xcc6699, 0xcc66cc, 0xcc66ff
    , 0xcc9900, 0xcc9933, 0xcc9966, 0xcc9999, 0xcc99cc, 0xcc99ff
    , 0xcccc00, 0xcccc33, 0xcccc66, 0xcccc99, 0xcccccc, 0xccccff
    , 0xccff00, 0xccff33, 0xccff66, 0xccff99, 0xccffcc, 0xccffff

    , 0xff0000, 0xff0033, 0xff0066, 0xff0099, 0xff00cc, 0xff00ff
    , 0xff3300, 0xff3333, 0xff3366, 0xff3399, 0xff33cc, 0xff33ff
    , 0xff6600, 0xff6633, 0xff6666, 0xff6699, 0xff66cc, 0xff66ff
    , 0xff9900, 0xff9933, 0xff9966, 0xff9999, 0xff99cc, 0xff99ff
    , 0xffcc00, 0xffcc33, 0xffcc66, 0xffcc99, 0xffcccc, 0xffccff
    , 0xffff00, 0xffff33, 0xffff66, 0xffff99, 0xffffcc, 0xffffff
    ]

paletteDelta :: Integer
paletteDelta = 0x33

rgbtoPaletteColor :: BmpPixel -> XpmPaletteColor
rgbtoPaletteColor (BmpPixel r g b) = paletteColor
  where
    r'  = toPaletteIndex r
    g'  = toPaletteIndex g
    b'  = toPaletteIndex b
    idx = toInteger r' * 36 + toInteger g' * 6 + toInteger b'
    paletteColor = xpmPalette !! fromInteger idx

toPaletteIndex :: Word8 -> Integer
toPaletteIndex c =
    if c' `mod` paletteDelta == 0
        then pos
        else paletteApprox c' pos
    where
      c'  = toInteger c
      pos = c' `div` paletteDelta

paletteApprox :: Integer -> Integer -> XpmPaletteColor
paletteApprox c pos =
    if d1 > d2 then pos+1 else pos
    where d1 = abs $ c - xpmPalette !! fromInteger pos
          d2 = abs $ c - xpmPalette !! fromInteger pos+1

-----------------------------------------------------------------------------
