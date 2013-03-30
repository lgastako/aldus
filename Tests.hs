module Main where

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework (defaultMain)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Aldus.Core(mkrel, destPath)


-- ** helpers ** --

-- | An infinite string of slashes to take from below.
slashes :: String
slashes = '/' : slashes

-- ** begin quickcheck tests ** --

-- For any relative path (that is a non-zero length strength that doesn't start
-- with slash or dot), mkrel with a dir of "/foo" should return "/foo/" + that
-- path.
prop_mkrel_drops_prefix :: String -> Property
prop_mkrel_drops_prefix s = isRelPath s ==> mkrel "/foo" ("/foo/" ++ s) == s
    where isRelPath p = length p > 0 && (head p) /= '/' && (head p) /= '.'

prop_destPath_moves :: String -> Bool
prop_destPath_moves s = destPath "/src" "/dst" ("/src/" ++ s) == "/dst/" ++ s

-- destPath should normalize any repeated slashes to a single slash, so for any
--     destPath "/src" "/dst" "/src////" + (whatever) the result should b "/dst/" + (whatever)
--     except in the case of "/src" or equivalent which should result in "/dst"

prop_destPath_drops_double_slash :: String -> Property
prop_destPath_drops_double_slash s =
    (s /= "") ==>
        destPath "/src" "/dst" ("/src////" ++ s) == "/dst/" ++ s

-- No matter how many slashes there are on the end of the exact srcDir
--
--prop_destPath_handles_src_equiv :: (Testable p, Int a) => a -> p
--prop_destPath_handles_src_equiv :: Int -> Property
prop_destPath_handles_src_equiv :: Int -> Bool
prop_destPath_handles_src_equiv n = destPath "/src" "/dst" ("/src" ++ s) == "/dst"
    where s = take n slashes

prop_destPath_drops_trailing_slash :: String -> Bool
prop_destPath_drops_trailing_slash s = destPath "/src" "/dst" ("/src/" ++ s ++ "/") == "/dst/" ++ s


rawQCTests :: [(String, String -> Property)]
rawQCTests = [ ( "mkrel drops prefix"
               , prop_mkrel_drops_prefix)
             , ( "destPath drops double slash"
               , prop_destPath_drops_double_slash)
             --, ( "destPath moves"
             --  , prop_destPath_moves)
             --, ( "destPath handles src equiv"
             --  , prop_destPath_handles_src_equiv)
             ]

quickcheckTests :: [Test]
quickcheckTests = map f rawQCTests
    where f = uncurry testProperty

-- ** end quickcheck tests ** --


-- ** begin hunit tests ** --

test_mkrelBasics = testCase "mkrel simple" $
    assertEqual "mkrel /home /home/bob -> bob"
        (mkrel "/home" "/home/bob")
        "bob"

test_mkrelSrcIsDst = testCase "mkrel src is dst" $
    assertEqual "mkrel /home /home/bob -> bob"
        (mkrel "/home" "/home")
        ""

--test_destPath

unitTests :: [Test]
unitTests = [ test_mkrelBasics
             , test_mkrelSrcIsDst
             ]

-- ** end hunit tests ** --

tests :: [Test]
tests = quickcheckTests ++ unitTests

-- ** end tests ** --

main :: IO ()
main = defaultMain tests

