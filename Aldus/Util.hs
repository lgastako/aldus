module Aldus.Util (parwalk) where


parwalk :: FilePath -> (FilePath -> String -> IO a) -> IO Integer
parwalk _dir _f = undefined


