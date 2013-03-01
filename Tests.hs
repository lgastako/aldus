module Main where

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Aldus.Core(mkrel,destPath)
import Data.String.Utils(join)


--slashes :: String

slashes = '/' : slashes

-- ** begin tests ** --

-- ** mkrel ** --

prop_mkrel_drops_prefix :: [Char] -> Property
prop_mkrel_drops_prefix s = length s > 0 && (head s) /= '/' && (head s) /= '.' ==>
    mkrel "/foo" ("/foo/" ++ s) == s


-- ** destPath ** --

prop_destPath_moves s = destPath "/src" "/dst" ("/src/" ++ s) == "/dst/" ++ s

-- destPath should normalize any repeated slashes to a single slash, so for any
--     destPath "/src" "/dst" "/src////" + (whatever) the result should b "/dst/" + (whatever)
--     except in the case of "/src" or equivalent which should result in "/dst"

prop_destPath_drops_double_slash s =
    (s /= "") ==>
        destPath "/src" "/dst" ("/src////" ++ s) == "/dst/" ++ s

-- No matter how many slashes there are on the end of the exact srcDir
prop_destPath_handles_src_equiv n = destPath "/src" "/dst" ("/src" ++ s)
    where s = take n slashes

prop_destPath_drops_trailing_slash s = destPath "/src" "/dst" ("/src/" ++ s ++ "/") == "/dst/" ++ s

-- ** end tests ** --

--tests :: [(String, ([Char] -> Property))]
tests = [ ( "mkrel drops prefix"          , prop_mkrel_drops_prefix)
        , ( "destPath drops double slash" , prop_destPath_drops_double_slash)
        --, ( "destPath moves"              , prop_destPath_moves)
        --, ( "destPath handles src equiv"  , prop_destPath_handles_src_equiv)
        ]

quickcheck_tests :: [Test]
quickcheck_tests = map f tests
    where f = uncurry testProperty

main :: IO ()
main = defaultMain quickcheck_tests

