------------------------------------------------------------------------------------
-- |
-- Module      :  Xpm
-- Copyright   :  (C) 2015 Sajith Sasidharan
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  sajith@nonzen.in
-- Stability   :  experimental
-- Portability :  unknown
--
------------------------------------------------------------------------------------

module Xpm where

-----------------------------------------------------------------------------

import qualified Data.Text.Lazy as T

import qualified Data.Map       as M
import           Data.Word      (Word32, Word8)

import           Data.Char      (chr)
import           Data.List      (group)
import           Text.Printf    (printf)

import           Bmp

-----------------------------------------------------------------------------

-- XPM v3 types.  Everything is a string!

type XpmData      = T.Text -- the whole file.

type XpmHeader    = T.Text -- rows, columns, colors, etc.
type XpmPixel     = T.Text -- two-character "pixels".
type XpmPixelRow  = T.Text -- row of pixels
type XpmColorRow  = T.Text -- "pp c #bbggrr" line.
type XpmBitmap    = T.Text -- the actual xpm bitmap.

type XpmColorMap  = M.Map XpmPaletteColor XpmPixel

-----------------------------------------------------------------------------

-- Using a static color palette here, to aid the translation from
-- RGB888 to 8-bit colors.  A better alternative would be to build a
-- color palette from a histogram of the source image's color space,
-- since that might give us a more faithful translation.  But we're
-- lazy.

type XpmPaletteColor = Word32

-- This is our 216-color palette.  (Why 216 colors? 6 choices each of
-- red, green, and blue gives us 6*6*6 = 216 colors.)
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

-- The `distance' between two adjacent colors in the palette.
paletteDelta :: Word8
paletteDelta = 0x33

-----------------------------------------------------------------------------

-- XPM pixels are chosen from this range of ASCII values.
xpmChrRange :: String
xpmChrRange = map chr [48..124]

-- TODO: rewrite this mawky stuff.
xpmPixels :: [XpmPixel]
xpmPixels = map T.pack $ oneLetters ++ twoLetters xpmChrRange
    where
        oneLetters = group xpmChrRange
        twoLetters :: String -> [String]
        twoLetters []     = group ""
        twoLetters (x:xs) = map (\c -> c ++ [x]) (group xs) ++ twoLetters xs

-----------------------------------------------------------------------------

-- One XPM "pixel" consists of two characters.
xpmCharsPerPixel :: Integer
xpmCharsPerPixel = 2

-- Our XPM files use a 216-color palette.
xpmNumColors :: Int
xpmNumColors = length xpmPalette

-----------------------------------------------------------------------------

xpmFormHeader :: BitmapName -> BmpInfoHeader -> XpmHeader
xpmFormHeader name info = T.pack $
    "/* XPM */\n"
    ++ "static char *" ++ name ++ "[] = {\n"
    ++ "/* columns rows colors chars-per-pixel */\n\""
    ++ show (imageWidth info) ++ " " ++ show (imageHeight info) ++ " "
    ++ show xpmNumColors ++ " " ++ show xpmCharsPerPixel ++ "\",\n"

-----------------------------------------------------------------------------

-- The palette-to-pixels lookup table.
xpmColorMap :: XpmColorMap
xpmColorMap = M.fromList $ zip xpmPalette xpmPixels

-----------------------------------------------------------------------------

-- Generate "pp c #rrggbb" lines.
xpmColorLines :: [XpmColorRow]
xpmColorLines = map (uncurry colorLine) $ M.toList xpmColorMap
    where
        -- TODO: Replace the printf here with format, perhaps.
        colorLine :: XpmPaletteColor -> XpmPixel -> XpmColorRow
        colorLine pc px = T.pack
                          $ printf "\"%2v c #%06X\",\n" (T.unpack px) pc

-----------------------------------------------------------------------------

