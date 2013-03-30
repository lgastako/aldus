module Aldus.Util (parwalk) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist,
                         getDirectoryContents)
import System.FilePath ((</>))


-- | Parallel walk of a directory.  Given the name of the directory
--   and a function that takes as arguments the filename and contents
--   of each file, walks the tree, calling f on every non-directory
--   and produces the number of files found.
--
--   TODO: Make parallel.
parwalk :: FilePath -> (FilePath -> String -> IO a) -> IO Int
parwalk top f = do
    names <- getDirectoryContents top
    let goodNames = filter (`notElem` [".", ".."]) names
    subcounts <- forM goodNames $ \name -> do
        let path = top </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then parwalk path f
            else do
                contents <- readFile path
                _ <- f path contents
                return 1
    return $ sum subcounts
