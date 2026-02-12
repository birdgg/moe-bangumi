module Moe.Infra.Metadata.BangumiData.TypesSpec (tests) where

import Moe.Domain.Bangumi (TmdbId (..))
import Moe.Infra.Metadata.BangumiData.Types
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Moe.Infra.Metadata.BangumiData.Types"
    [ testGroup "extractTmdbId" (map mkTest testData)
    ]

data TestCase = TestCase
  { name :: String,
    sites :: [BangumiDataSite],
    expected :: Maybe TmdbId
  }

testData :: [TestCase]
testData =
  [ TestCase
      "tv path format"
      [BangumiDataSite "tmdb" "tv/209867"]
      (Just (TmdbId 209867)),
    TestCase
      "tv path with season format"
      [BangumiDataSite "tmdb" "tv/237529/season/2"]
      (Just (TmdbId 237529)),
    TestCase
      "plain numeric id"
      [BangumiDataSite "tmdb" "12345"]
      (Just (TmdbId 12345))
  ]

mkTest :: TestCase -> TestTree
mkTest tc = testCase tc.name $ do
  extractTmdbId tc.sites @?= tc.expected
