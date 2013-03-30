module Aldus.Core where

import Control.Monad
import System.FilePath(joinPath,makeRelative,pathSeparator)
import Data.List(stripPrefix)
import Data.Maybe(fromJust)
import Aldus.Config
import Aldus.Util(parwalk)


-- fixes makeRelative to give it the behavior I want.
mkrel :: FilePath -> FilePath -> FilePath
mkrel dir fn
    | path == joinPath [dir, "."] = ""
    | path == "."                 = ""
    | otherwise                   = path
    where path = makeRelative dir fn


destPath :: FilePath -> FilePath -> FilePath -> FilePath
destPath srcDir dstDir "" = dstDir
destPath srcDir dstDir srcFile
    | srcFile == srcDir ++ [pathSeparator] = dstDir ++ [pathSeparator]
    | otherwise                            = joinPath [dstDir, fn]
    where
        fn = mkrel srcDir srcFile


publish :: FilePath -> (String -> String) -> FilePath -> IO ()
publish inPath f outPath = do
    originalContents <- readFile inPath
    let publishedContents = f originalContents
    writeFile outPath publishedContents


publishFile :: FilePath -> FilePath -> FilePath -> IO ()
publishFile srcDir dstDir srcFile = do
    let dstPath = destPath srcDir dstDir srcFile
    return ()


publishDir :: AldusConfig -> IO Integer
publishDir cfg = do
    let srcDir = inputDirectory cfg
    let dstDir = outputDirectory cfg
    let pub path contents = publishFile srcDir dstDir path
    numFiles <- parwalk srcDir pub
    return numFiles

-- parwalk needs to walk every file in srcDir and emit pairs of
-- (filename, fileContents) to publishFile

