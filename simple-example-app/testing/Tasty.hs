module Taste where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import Control.Exception

test = testCase "Test 1" (2 @?= 3)

main = defaultMain test
  `catch` (\e -> do
    if e == ExitSuccess
      then putStrLn "Yea"
      else putStrLn "Nay"
    throwIO e)
