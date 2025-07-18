{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified System.Directory as Dir
import qualified System.Process as Proc

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "qualified"
    [ testCase "1" $ runTest "Case1.hs"
    , testCase "2" $ runTest "Case2.hs"
    , testCase "3" $ runTest "Case3.hs"
    , testCase "4" $ runTest "Case4.hs"
    , testCase "5" $ runTest "Case5.hs"
    , testCase "6" $ runTest "Case6.hs"
    , testCase "7" $ runTest "Case7.hs"
    , testCase "8" $ runTest "Case8.hs"
    , testCase "25" $ runTest "Case25.hs"
    ]
  , testGroup "unqualified"
    [ testCase "9" $ runTest "Case9.hs"
    , testCase "10" $ runTest "Case10.hs"
    , testCase "11" $ runTest "Case11.hs"
    , testCase "12" $ runTest "Case12.hs"
    , testCase "13" $ runTest "Case13.hs"
    , testCase "14" $ runTest "Case14.hs"
    , testCase "15" $ runTest "Case15.hs"
    , testCase "16" $ runTest "Case16.hs"
    , testCase "17" $ runTest "Case17.hs"
    , testCase "18" $ runTest "Case18.hs"
    , testCase "19" $ runTest "Case19.hs"
    , testCase "20" $ runTest "Case20.hs"
    , testCase "21" $ runTest "Case21.hs"
    , testCase "22" $ runTest "Case22.hs"
    , testCase "23" $ runTest "Case23.hs"
    , testCase "24" $ runTest "Case24.hs"
    ]
  ]

testModulePath :: String -> FilePath
testModulePath name = "test-modules/" <> name

-- copy the input file contents to the module file to be compiled
prepTest :: FilePath -> IO ()
prepTest modFile = do
  inp <- readFile (modFile ++ ".input")
  writeFile modFile inp

runTest :: FilePath -> Assertion
runTest name = do
  let modFile = testModulePath name
  prepTest modFile
  (_, _, _, h) <- Proc.createProcess $
    Proc.proc "cabal" ["build", "test-modules:" ++ takeWhile (/= '.') name]
  void $ Proc.waitForProcess h
  updatedMod <- readFile modFile
  expectedMod <- readFile $ modFile ++ ".expected"
  assertEqual "Expected update" expectedMod updatedMod
  Dir.removeFile modFile
