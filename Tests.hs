import Test.QuickCheck
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Aldus.Core(mkrel,destPath)


-- mkrel
prop_mkrel_drops_prefix s = length s > 0 && (head s) /= '/' && (head s) /= '.' ==>
    mkrel "/foo" ("/foo/" ++ s) == s


-- destPath
prop_destPath_moves s = destPath "/src" "/dst" ("/src/" ++ s) == "/dst/" ++ s
prop_destPath_drops_double_slash s = destPath "/src" "/dst" ("/src////" ++ s) == "/dst/" ++ s
prop_destPath_drops_trailing_slash s = destPath "/src" "/dst" ("/src/" ++ s ++ "/") == "/dst/" ++ s



-- test suite definitions, etc.  todo: automate

tests = [ testProperty "mkrel_drops_prefix" prop_mkrel_drops_prefix
        , testProperty "destPath_moves" prop_destPath_moves
        , testProperty "destPath_drops_double_slash" prop_destPath_drops_double_slash
        , testProperty "destPath_drops_trailing_slash" prop_destPath_drops_trailing_slash
        ]

main = defaultMain tests

