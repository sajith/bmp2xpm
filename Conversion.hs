------------------------------------------------------------------------------------
-- |
-- Module      :  Conversion
-- Copyright   :  (C) 2015 Sajith Sasidharan
-- License     :  BSD3 (see LICENSE)
--
-- Maintainer  :  sajith@nonzen.in
-- Stability   :  experimental
-- Portability :  unknown
--
------------------------------------------------------------------------------------

module Conversion where

-----------------------------------------------------------------------------

import qualified Data.Map                    as M
import qualified Data.Text.Lazy              as T
import           Data.Word                   (Word8)

-- In theory we need only one of 'Formatting' and 'Text.Printf', but
-- in practice I don't know how to correctly format XPM palette using
-- 'Formatting'.  But 'Formatting' performs so much better, so using
-- it where possible is a good idea.
import           Formatting                  as F

import qualified Control.DeepSeq             as D
import qualified Control.Parallel.Strategies as P

import qualified Data.Text.Lazy.IO           as T (hPutStr)
import           System.FilePath             (takeBaseName)
import           System.IO                   (IOMode (ReadMode, WriteMode),
                                              withBinaryFile, withFile)

import           Bmp
import           Xpm

-----------------------------------------------------------------------------

-- Convert BMP file to XPM file.
bmpToXpm :: BitmapName -> BmpFile -> XpmData
bmpToXpm name (BmpFile _ info bitmap) = xpmData
    where
        xpmLeader = xpmFormHeader name info
        colorStr  = T.pack "/* colors */\n"
        xpmColors = T.append colorStr (T.concat xpmColorLines)
        xpmHeader = T.append xpmLeader xpmColors
        pixelStr  = T.pack "/* pixels */\n"
        xpmBitmap = T.append pixelStr (translateBitmap bitmap)
        xpmBody   = T.append xpmHeader xpmBitmap
        xpmData   = T.append xpmBody (T.pack "\n};")

-----------------------------------------------------------------------------

-- Convert a BMP pixel to a color in our palette.
toPaletteColor :: BmpPixel -> XpmPaletteColor
toPaletteColor (BmpPixel b g r) = xpmPalette !! fromEnum idx
    where
        -- BGR -> RGB, sort of.
        idx = paletteIndex r * 36 + paletteIndex g * 6 + paletteIndex b
{-# INLINE toPaletteColor #-}

-- Find palette position from the given color intensity.
paletteIndex :: Word8 -> Word8
paletteIndex c =
    if c `mod` paletteDelta == 0
        then pos
        else paletteApprox c pos
    where
        pos = c `div` paletteDelta
{-# INLINE paletteIndex #-}

-- Find the closest palette color.
paletteApprox :: Word8 -> Word8 -> Word8
paletteApprox c pos =
    if d1 > d2 then pos+1 else pos
    where d1 = abs $ fromEnum c - fromEnum (xpmPalette !! fromEnum pos)
          d2 = abs $ fromEnum c - fromEnum (xpmPalette !! fromEnum (pos+1))
{-# INLINE paletteApprox #-}

-----------------------------------------------------------------------------

-- 'splitBy' (originally 'split') and 'chunk' are from Simon Marlow's
-- "Parallel and Concurrent Programming in Haskell", 1st ed, pp 40.
splitBy :: Int -> [a] -> [[a]]
splitBy n xs = chunk (length xs `quot` n) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs
    where
        (as, bs) = splitAt n xs

-- Good news: threadscope shows parallelism, and productivity now is
-- "82.7% of total user, 280.4% of total elapsed"
translateBitmap :: BmpBitmap -> XpmBitmap
translateBitmap rows = T.intercalate (T.pack ",\n") res
    where
        [a, b, c, d] = splitBy 4 rows
        res = P.runEval $ do
            a' <- P.rpar $ D.force $ map translatePixelRow a
            b' <- P.rpar $ D.force $ map translatePixelRow b
            c' <- P.rpar $ D.force $ map translatePixelRow c
            d' <- P.rseq $ D.force $ map translatePixelRow d
            return (a' ++ b' ++ c' ++ d')

-----------------------------------------------------------------------------

-- The commented out stuff below is of historic interest: the first
-- one is the original sequential version of translateBitmap.
--
-- The second one answered the question "what if I use rpar/rseq this
-- way?"  And the answer was: that would actually be sequential, you
-- silly person!

{--

-- Translate from BMP bitmap to XPM bitmap, the sequential version.
translateBitmap :: BmpBitmap -> XpmBitmap
translateBitmap rows = T.intercalate (T.pack ",\n")
                       $ map translatePixelRow rows

--}

{--

-- First attempt with rpar/rseq.
translateBitmap :: BmpBitmap -> XpmBitmap
translateBitmap rows = T.intercalate (T.pack ",\n") $ map parPixelRow rows

-- This is essentially sequential!
parPixelRow :: BmpPixelRow -> XpmPixelRow
parPixelRow row = P.runEval $ do
    new <- P.rpar $ D.force $ translatePixelRow row
    _   <- P.rseq new
    return new

--}

-----------------------------------------------------------------------------

-- Translate a row of pixels.
translatePixelRow :: BmpPixelRow -> XpmPixelRow
translatePixelRow row = quote $ T.concat $ map translatePixel row

-----------------------------------------------------------------------------

-- XXX: This function is the hot-spot.  How can I improve it?
translatePixel :: BmpPixel -> XpmPixel
translatePixel p = F.format (left 2 ' ' %. text)
                   $ xpmColorMap M.! toPaletteColor p

-----------------------------------------------------------------------------

-- Put double quotes around text.
quote :: T.Text -> T.Text
quote txt = T.snoc (T.cons dq txt) dq where dq = '"'

-----------------------------------------------------------------------------

runConversion :: FilePath -> FilePath -> IO ()
runConversion infile outfile = do
    let name    = takeBaseName infile

    -- TODO: Leaves an empty output file when conversion fails. Fix.
    withBinaryFile infile ReadMode
        (\inh -> do
              bmpdata <- readBmpFile inh
              withFile outfile WriteMode
                  (\outh -> T.hPutStr outh (bmpToXpm name bmpdata)))

-----------------------------------------------------------------------------
