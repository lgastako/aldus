module Main(main) where

import Aldus.Config(defaultConfig,altConfig)
import Aldus.Core(publishDir)
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    config <- case args of
        [dir] -> defaultConfig dir
        ["--config", configFilename, dir] -> altConfig dir configFilename
        _ -> error "Usage: aldus [--config filename] topDir"
    numFiles <- publishDir config
    putStrLn ("Published " ++ (show numFiles) ++ " files.")

