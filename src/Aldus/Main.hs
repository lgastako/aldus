module Main(main) where

import Aldus.Config(defaultConfig,altConfig)
import Aldus.Core(publishConfig)
import System.Environment(getArgs)


main :: IO ()
main = do
    args <- getArgs
    config <- case args of
        [dir] -> defaultConfig dir
        ["--config", configFilename, dir] -> altConfig dir configFilename
        _ -> error "Usage: aldus [--config filename] topDir"
    numFiles <- publishConfig config
    putStrLn ("Published " ++ (show numFiles) ++ " files.")
