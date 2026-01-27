module Main (main) where

import Moe.File.Naming
import Moe.File.Parser
import Moe.File.Types
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ testGroup "Naming Tests" namingTests,
      testGroup "Sanitization Tests" sanitizationTests,
      testGroup "Title Parser Tests" titleParserTests
    ]

namingTests :: [TestTree]
namingTests =
  [ testCase "regular episode S01E01" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode (Regular 1 1)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.mkv",
    testCase "regular episode S02E15" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode (Regular 2 15)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/Season 02/Frieren - S02E15.mkv",
    testCase "special episode S00E01" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode (Special 1)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/Season 00/Frieren - S00E01.mkv",
    testCase "NCOP without index" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Extra (NCOP Nothing)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/extras/NCOP.mkv",
    testCase "NCOP with index" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Extra (NCOP (Just 2))) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/extras/NCOP2.mkv",
    testCase "NCED without index" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Extra (NCED Nothing)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/extras/NCED.mkv",
    testCase "NCED with index" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Extra (NCED (Just 3))) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/extras/NCED3.mkv",
    testCase "Menu without volume" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Extra (Menu Nothing)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/extras/Menu.mkv",
    testCase "Menu with volume" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Extra (Menu (Just 1))) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/extras/Menu1.mkv",
    testCase "PV trailer" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (TrailerItem (PV 1)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/trailers/PV1.mkv",
    testCase "Preview trailer" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (TrailerItem Preview) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/trailers/Preview.mkv",
    testCase "Trailer" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (TrailerItem Trailer) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/trailers/Trailer.mkv",
    testCase "CM without index" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (TrailerItem (CM Nothing)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/trailers/CM.mkv",
    testCase "CM with index" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (TrailerItem (CM (Just 2))) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/trailers/CM2.mkv",
    testCase "movie" $ do
      let meta = BangumiMeta "Frieren Movie" Nothing Nothing
          file = BangumiFile meta (Movie 2025) (Video MKV)
      generateFullPath file @?= "Frieren Movie (2025)/Frieren Movie (2025).mkv",
    testCase "subtitle file CHS" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode (Regular 1 1)) (Subtitle CHS ASS)
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.chs.ass",
    testCase "subtitle file JPN" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode (Regular 1 1)) (Subtitle JPN SRT)
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.jpn.srt",
    testCase "no year in meta" $ do
      let meta = BangumiMeta "Frieren" Nothing Nothing
          file = BangumiFile meta (Episode (Regular 1 1)) (Video MKV)
      generateFullPath file @?= "Frieren/Season 01/Frieren - S01E01.mkv",
    testCase "with TMDB ID" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) (Just 12345)
          file = BangumiFile meta (Episode (Regular 1 1)) (Video MKV)
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.mkv",
    testCase "MP4 extension" $ do
      let meta = BangumiMeta "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode (Regular 1 1)) (Video MP4)
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.mp4"
  ]

titleParserTests :: [TestTree]
titleParserTests =
  [ testCase "parse standard format" $ do
      let result = parseTitle "[北宇治字幕组] 葬送的芙莉莲 / Sousou no Frieren [29][WebRip][1080p][HEVC_AAC][简繁日内封]"
      fmap (.episode) result @?= Just 29
      fmap (.subtitleGroup) result @?= Just (Just "北宇治")
      fmap (sort . (.subtitles)) result @?= Just (sort [CHS, CHT, JPN]),
    testCase "parse S01E format" $ do
      let result = parseTitle "[SweetSub] Anime Name S01E05 [1080p]"
      fmap (.episode) result @?= Just 5
      fmap (.subtitleGroup) result @?= Just (Just "SweetSub"),
    testCase "parse EP format" $ do
      let result = parseTitle "[LoliHouse] Anime EP12 [CHS]"
      fmap (.episode) result @?= Just 12
      fmap (.subtitles) result @?= Just [CHS],
    testCase "parse END format" $ do
      let result = parseTitle "[字幕组] 动画名 [13.END][1080p][简日]"
      fmap (.episode) result @?= Just 13,
    testCase "parse 第X话 format" $ do
      let result = parseTitle "[字幕组] 动画名 第05话 [1080p]"
      fmap (.episode) result @?= Just 5,
    testCase "extract group with full-width brackets" $ do
      let result = parseTitle "【北宇治字幕组】动画名 [01]"
      fmap (.subtitleGroup) result @?= Just (Just "北宇治"),
    testCase "group normalization - alias" $ do
      let result = parseTitle "[❀拨雪寻春❀] 动画名 [01]"
      fmap (.subtitleGroup) result @?= Just (Just "拨雪寻春"),
    testCase "extract CHT subtitle" $ do
      let result = parseTitle "[字幕组] 动画名 [01][繁体]"
      fmap (.subtitles) result @?= Just [CHT],
    testCase "extract ENG subtitle" $ do
      let result = parseTitle "[字幕组] 动画名 [01][ENG]"
      fmap (.subtitles) result @?= Just [ENG],
    testCase "no episode returns Nothing" $ do
      let result = parseTitle "[字幕组] 动画名 无集数"
      result @?= Nothing
  ]

sanitizationTests :: [TestTree]
sanitizationTests =
  [ testCase "colon replaced with dash" $ do
      sanitizeName "Re:Zero" @?= "Re-Zero",
    testCase "forward slash replaced with dash" $ do
      sanitizeName "Fate/Zero" @?= "Fate-Zero",
    testCase "backslash replaced with dash" $ do
      sanitizeName "Test\\Name" @?= "Test-Name",
    testCase "forbidden chars removed" $ do
      sanitizeName "Test<>|?*\"Name" @?= "TestName",
    testCase "mixed special chars" $ do
      sanitizeName "Re:Zero / Fate<Test>" @?= "Re-Zero - FateTest",
    testCase "no changes needed" $ do
      sanitizeName "Normal Name" @?= "Normal Name",
    testCase "full path with special chars" $ do
      let meta = BangumiMeta "Re:Zero" (Just 2016) Nothing
          file = BangumiFile meta (Episode (Regular 1 1)) (Video MKV)
      generateFullPath file @?= "Re-Zero (2016)/Season 01/Re-Zero - S01E01.mkv"
  ]
