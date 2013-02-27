module Aldus.Config(AldusConfig, defaultConfig, altConfig, inputDirectory, outputDirectory) where

import Text.JSON(decodeStrict,
                 Result(Ok,
                        Error),
                 valFromObj)
import System.FilePath.Posix(joinPath)


data AldusConfig = AldusConfig { inputDirectory :: FilePath
                               , outputDirectory :: FilePath
                               , excludes :: [FilePath]
                               }
                               deriving (Show, Eq)

parseConfig :: FilePath -> String -> Result AldusConfig
parseConfig inDir jsonText = let (!) = flip valFromObj in do
    json <- decodeStrict jsonText
    outDir <- json ! "outputLocation"
    return AldusConfig { inputDirectory=inDir
                       , outputDirectory=outDir
                       , excludes=[]
                       }

readConfig :: FilePath -> FilePath -> IO AldusConfig
readConfig srcDir filename = do
    s <- readFile filename
    case (parseConfig srcDir s) of
        Ok config -> return config
        Error msg -> error msg

defaultConfig :: FilePath -> IO AldusConfig
defaultConfig dir = readConfig dir $ joinPath [dir, "alduscfg.json"]

altConfig :: FilePath -> FilePath -> IO AldusConfig
altConfig dir configFilename = readConfig dir configFilename

