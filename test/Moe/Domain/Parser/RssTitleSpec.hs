module Moe.Domain.Parser.RssTitleSpec (tests) where

import Moe.Domain.Shared.Group (GroupName (..))
import Moe.Domain.Setting (defaultGroupPriority)
import Moe.Domain.Episode (EpisodeNumber (..))
import Moe.Domain.Shared.Subtitle (Subtitle (..))
import Moe.Domain.Parser.RssTitle
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Moe.Domain.Parser.RssTitle"
    [ testGroup "parseRssTitle" (map mkTest testData)
    ]

data TestCase = TestCase
  { name :: String,
    input :: Text,
    expectedEpisode :: Maybe EpisodeNumber,
    expectedGroup :: [GroupName],
    expectedResolution :: Maybe Text,
    expectedSubtitleList :: [Subtitle]
  }

testData :: [TestCase]
testData =
  [ TestCase
      "Basic format"
      "[Nekomoe kissaten][Lycoris Recoil][01][1080p][JPSC].mp4"
      (Just (EpisodeNumber 1))
      [GroupName "Nekomoe kissaten"]
      (Just "1080P")
      [CHS, JPN],
    TestCase
      "With dash episode"
      "[ANi] Spy x Family S2 - 01 [1080P][Baha][WEB-DL][AAC AVC][CHT].mp4"
      (Just (EpisodeNumber 1))
      [GroupName "ANi"]
      (Just "1080P")
      [CHT],
    TestCase
      "Multiple groups split by &"
      "[动漫国字幕组&LoliHouse] THE MARGINAL SERVICE - 08 [WebRip 1080p HEVC-10bit AAC][简繁内封字幕].mp4"
      (Just (EpisodeNumber 8))
      [GroupName "动漫国字幕组", GroupName "LoliHouse"]
      (Just "1080P")
      [CHS, CHT],
    TestCase
      "Chinese and Japanese subs"
      "[喵萌奶茶屋&LoliHouse] 药屋少女的呢喃 / 药师少女的独语 - 01 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕].mp4"
      (Just (EpisodeNumber 1))
      [GroupName "喵萌奶茶屋", GroupName "LoliHouse"]
      (Just "1080P")
      [CHS, CHT, JPN],
    TestCase
      "Bracket episode"
      "[桜都字幕组] 葬送のフリーレン / Sousou no Frieren [01][1080p][简繁内封].mp4"
      (Just (EpisodeNumber 1))
      [GroupName "桜都字幕组"]
      (Just "1080P")
      [CHS, CHT],
    TestCase
      "With END marker"
      "[Sakurato] 我的推是坏人大小姐 / Watashi no Oshi wa Akuyaku Reijou [13.END][1080p][简繁内封].mp4"
      (Just (EpisodeNumber 13))
      [GroupName "Sakurato"]
      (Just "1080P")
      [CHS, CHT],
    TestCase
      "4K resolution"
      "[NC-Raws] 咒术回战 第二季 / Jujutsu Kaisen S2 - 21 [B-Global][WEB-DL][2160p][HDR][HEVC-10bit][AAC][Multi-Audio][Multi-Subs].mp4"
      (Just (EpisodeNumber 21))
      [GroupName "NC-Raws"]
      (Just "2160P")
      [],
    TestCase
      "Bracket episode 06"
      "[Strange-Raw] 战队红战士在异世界当冒险者 第一季 / Sentai Red Isekai de Boukensha ni Naru S01 [06] [Bilibili] [WEB-DL] [1080P AVC-8Bits AAC 2.0] [简日内嵌字幕]"
      (Just (EpisodeNumber 6))
      [GroupName "Strange-Raw"]
      (Just "1080P")
      [CHS, JPN],
    TestCase
      "Dash without space"
      "[Skymoon-Raws] 我独自升级 第二季 -起于暗影- / Ore dake Level Up na Ken Season 2 -10 [ViuTV][WEB-DL][1080p][AVC AAC]"
      (Just (EpisodeNumber 10))
      [GroupName "Skymoon-Raws"]
      (Just "1080P")
      [],
    TestCase
      "Parentheses resolution"
      "[Up to 21°C] 战队红战士在异世界当冒险者 / Sentai Red Isekai de Boukensha ni Naru - 09 (CR 1920x1080 AVC AAC MKV)"
      (Just (EpisodeNumber 9))
      [GroupName "Up to 21°C"]
      (Just "1080P")
      [],
    TestCase
      "Chinese only"
      "[ANi]  超超超超超喜欢你的 100 个女朋友 - 21 [1080P][Baha][WEB-DL][AAC AVC][CHT][MP4]"
      (Just (EpisodeNumber 21))
      [GroupName "ANi"]
      (Just "1080P")
      [CHT],
    TestCase
      "Dash no space before episode"
      "[AnimeRep] 一个人的异世界攻略 / Loner Life in Another World- 01 [1080p][简中内嵌]"
      (Just (EpisodeNumber 1))
      [GroupName "AnimeRep"]
      (Just "1080P")
      [CHS],
    TestCase
      "Episode followed by bracket"
      "[动漫国字幕组&LoliHouse] THE MARGINAL SERVICE - 08[WebRip 1080p HEVC AAC][简繁内封字幕].mp4"
      (Just (EpisodeNumber 8))
      [GroupName "动漫国字幕组", GroupName "LoliHouse"]
      (Just "1080P")
      [CHS, CHT],
    TestCase
      "720P resolution with triple groups"
      "【DHR动研字幕组&KNA字幕组&动漫国字幕组】[魔物娘的同居日常_Everyday Life with Monster Girls] - 12完 [繁体][720P][MP4]"
      (Just (EpisodeNumber 12))
      [GroupName "DHR动研字幕组", GroupName "KNA字幕组", GroupName "动漫国字幕组"]
      (Just "720P")
      [CHT, JPN]
  ]

mkTest :: TestCase -> TestTree
mkTest tc = testCase tc.name $ do
  let result = parseRssTitle defaultGroupPriority tc.input
  result.episode @?= tc.expectedEpisode
  result.group @?= tc.expectedGroup
  result.resolution @?= tc.expectedResolution
  result.subtitleList @?= tc.expectedSubtitleList
