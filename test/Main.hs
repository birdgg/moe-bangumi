module Main (main) where

import Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ testGroup
        "Unit Tests"
        [ testCase "example test" $ do
            1 + 1 @?= 2
        ]
    ]
