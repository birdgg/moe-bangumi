module Moe.Domain.Bangumi.Parser.BgmtvSpec (tests) where

import Data.Text qualified as T
import Moe.Domain.Bangumi.Parser.Bgmtv
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Moe.Domain.Bangumi.Parser.Bgmtv"
    [ testGroup "parseBgmtvTitle" (map mkTest testData)
    ]

data TestCase = TestCase
  { name :: String,
    input :: (String, String),
    expectedChs :: String,
    expectedJap :: String,
    expectedSeason :: Maybe Word32
  }

testData :: [TestCase]
testData =
  [ TestCase
      "Chinese season number preferred"
      ("葬送のフリーレン Season 2", "葬送的芙莉莲 第二季")
      "葬送的芙莉莲"
      "葬送のフリーレン"
      (Just 2),
    TestCase
      "Arabic numeral in Chinese title"
      ("無職転生 S02", "无职转生 第2季")
      "无职转生"
      "無職転生"
      (Just 2),
    TestCase
      "no season number"
      ("Test Anime", "测试动画")
      "测试动画"
      "Test Anime"
      Nothing,
    TestCase
      "Chinese numeral season 3"
      ("仙王的日常生活 第二季", "仙王的日常生活")
      "仙王的日常生活"
      "仙王的日常生活"
      (Just 2)
  ]

mkTest :: TestCase -> TestTree
mkTest tc = testCase tc.name $ do
  let (nameJp, nameCn) = tc.input
      BgmtvParsedTitle chs jap season = parseBgmtvTitle (T.pack nameJp, T.pack nameCn)
  chs @?= T.pack tc.expectedChs
  jap @?= T.pack tc.expectedJap
  season @?= tc.expectedSeason
