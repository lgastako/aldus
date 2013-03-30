module Main where

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework (defaultMain)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import System.FilePath ((</>))
import System.Directory (createDirectory)
import System.IO.Temp (withSystemTempDirectory)

import Aldus.Core(mkrel, destPath)
import Aldus.Util(parwalk)


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

test_mkrelBasics :: Test
test_mkrelBasics = testCase "mkrel basics" $ do
    assertEqual "mkrel /home /home/bob -> bob"
        "bob"
        (mkrel "/home" "/home/bob")
    assertEqual "mkrel /home /home/dave -> dave"
        "dave"
        (mkrel "/home" "/home/dave")

test_mkrelSrcIsDst :: Test
test_mkrelSrcIsDst = testCase "mkrel src is dst" $
    assertEqual "mkrel /home /home/bob -> bob"
        ""
        (mkrel "/home" "/home")

test_destPathBasics :: Test
test_destPathBasics = testCase "destPath basics" $ do
    assertEqual "destPath /a /a/b /c -> /c/b"
        "/c/b"
        (destPath "/a" "/c" "/a/b")
    assertEqual "destPath /x /x/y /z -> /z/y"
        "/z/y"
        (destPath "/x" "/z" "/x/y")

test_destPathSrcFileIsSrcDir :: Test
test_destPathSrcFileIsSrcDir = testCase "destPath srcFile is srcDir" $ do
    assertEqual "destPath /s /d /s/ -> /d"
        "/d/"
        (destPath "/s" "/d" "/s/")

test_parwalkSimple :: Test
test_parwalkSimple = testCase "parwalk basics" $ do
    withSystemTempDirectory "parwalk.basics." $ \dir -> do
        writeFile (dir </> "foo.txt") "foo"
        c1 <- parwalk dir $ (\_ _ -> return ())
        assertEqual "num files parwalk'd" 1 c1
        writeFile (dir </> "bar.txt") "bar"
        c2 <- parwalk dir $ (\_ _ -> return ())
        assertEqual "num files parwalk'd" 2 c2

test_parwalkDoesNotCountDirectories :: Test
test_parwalkDoesNotCountDirectories = testCase "parwalk does not count dirs" $ do
    withSystemTempDirectory "parwalk.countdirs." $ \dir -> do
        writeFile (dir </> "foo.txt") "foo"
        createDirectory (dir </> "bar")
        c <- parwalk dir $ (\_ _ -> return ())
        assertEqual "num files parwalk'd" 1 c


unitTests :: [Test]
unitTests = [ test_mkrelBasics
            , test_mkrelSrcIsDst
            , test_destPathBasics
            , test_destPathSrcFileIsSrcDir
            , test_parwalkSimple
            , test_parwalkDoesNotCountDirectories
            ]

-- ** end hunit tests ** --

tests :: [Test]
tests = quickcheckTests ++ unitTests

-- ** end tests ** --

main :: IO ()
main = defaultMain tests

