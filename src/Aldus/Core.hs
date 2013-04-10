module Aldus.Core where

import System.FilePath(joinPath,makeRelative,pathSeparator)
import Aldus.Config
import Aldus.Util(parwalk)


-- | Given a directory and a file within that directory, returns the
--   relative portion of the file's path.
--
-- Basically this fixes makeRelative to give it the behavior I want.
--
mkrel :: FilePath -> FilePath -> FilePath
mkrel dir fn
    | path == joinPath [dir, "."] = ""
    | path == "."                 = ""
    | otherwise                   = path
    where path = makeRelative dir fn


-- | Given source directory, a destination directory and the name of
--   a file in the source directory, returns the destination path
--   the file should be written to.
destPath :: FilePath -> FilePath -> FilePath -> FilePath
destPath _ dstDir "" = dstDir
destPath srcDir dstDir srcFile
    | srcFile == srcDir ++ [pathSeparator] = dstDir ++ [pathSeparator]
    | otherwise                            = joinPath [dstDir, fn]
    where
        fn = mkrel srcDir srcFile

-- | Given the path to an input file, a function to "compile" that
--   file and a destination to write it to and does those things.
publish :: FilePath -> (String -> String) -> FilePath -> IO ()
publish inPath f outPath = do
    originalContents <- readFile inPath
    let publishedContents = f originalContents
    writeFile outPath publishedContents


-- | TBD
publishFile :: FilePath -> FilePath -> FilePath -> IO ()
publishFile _srcDir _dstDir _srcFile = undefined
    --do
    --let _dstPath = destPath srcDir dstDir srcFile
    --return ()


-- | Given a configuration, publishes the entire site, returning
--   a count of the number of files that were published.
publishConfig :: AldusConfig -> IO Int
publishConfig cfg = do
    let srcDir = inputDirectory cfg
    let dstDir = outputDirectory cfg
    let pub path _contents = publishFile srcDir dstDir path
    numFiles <- parwalk srcDir pub
    return numFiles

-- parwalk needs to walk every file in srcDir and emit pairs of
-- (filename, fileContents) to publishFile

