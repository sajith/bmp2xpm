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
-- Nothing fancy here: Only 24-bit BGR888 pixels are supported.
-- Compression or more than one planes or ICC color profiles are not
-- supported.
--
-- That is silly enough, and something ImageMagick can trivially do.
-- My intention however is to teach myself some Haskell parallelism.
--
------------------------------------------------------------------------------------

{--

 TODO:

  - Try using memoization.  (The few libraries I tried actually slowed
    down things, likely because of fine granularity.  But this is a case
    flush with memoization opportunities!  I believe some hand-tuned
    memoization will help.)
  - Try alternate implementation with Repa.
  - Try DPH/Stream fusion.
  - Measure space usage; use pipes/conduit, if necessary.
  - Handle scanline padding, if present.
  - Add support for more complex (8/16/32-bit colors, more than one
    plane, newer/older BMP versions) input.
  - Use a histogram-based palette, rather than a static fixed palette.
  - Use exceptions instead of 'error'.
  - Use Strategies.
  - Use Critereon for benchmarking.

--}

------------------------------------------------------------------------------------

module Main (main) where

import           Control.Monad      (unless, when)

import           System.Directory   (doesFileExist, getPermissions, readable,
                                     writable)
import           System.Environment (getArgs, getProgName)
import           System.FilePath    (replaceExtension)
import           System.IO          (hPutStrLn, stderr)

import           Conversion         (runConversion)

------------------------------------------------------------------------------------

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
                 "input should be an uncompressed 24-bit Windows bitmap file."

-----------------------------------------------------------------------------

process :: FilePath -> FilePath -> IO ()
process infile outfile = do

    e1 <- isFileReadable infile
    unless e1 $ error $ "Can't read input file " ++ show infile

    e2 <- isFileWritable outfile
    unless e2 $ error $ "Can't write to output file " ++ show outfile

    runConversion infile outfile

    putStrLn $ infile ++ " -> " ++ outfile ++ " conversion done."

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

