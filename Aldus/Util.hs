module Aldus.Util (parwalk) where


parwalk :: FilePath -> (FilePath -> String -> IO a) -> IO Integer
parwalk dir f = do
    return 5


