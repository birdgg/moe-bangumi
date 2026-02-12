module Moe.Domain.FileSpec (tests) where

import Moe.Domain.File
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

-- | Helper to construct BangumiMeta for tests.
mkBangumi :: Text -> Maybe Year -> Maybe TmdbId -> BangumiMeta
mkBangumi title yr tid =
  BangumiMeta
    { title = title,
      year = yr,
      tmdbId = tid,
      season = Just (SeasonNumber 1),
      group = [],
      isBDrip = False
    }

tests :: TestTree
tests =
  testGroup
    "Moe.Domain.File"
    [ testGroup "generateFullPath" namingTests,
      testGroup "sanitizeName" sanitizationTests
    ]

namingTests :: [TestTree]
namingTests =
  [ testCase "regular episode S01E01" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode 1) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.mkv",
    testCase "regular episode S02E15" $ do
      let meta = (mkBangumi "Frieren" (Just 2023) Nothing) {season = Just (SeasonNumber 2)}
          file = BangumiFile meta (Episode 15) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 02/Frieren - S02E15.mkv",
    testCase "special episode S00E01" $ do
      let meta = (mkBangumi "Frieren" (Just 2023) Nothing) {season = Just (SeasonNumber 0)}
          file = BangumiFile meta (Episode 1) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 00/Frieren - S00E01.mkv",
    testCase "episode with group" $ do
      let meta = (mkBangumi "Frieren" (Just 2023) Nothing) {group = [GroupName "SweetSub"]}
          file = BangumiFile meta (Episode 1) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01 [SweetSub].mkv",
    testCase "episode with multiple groups" $ do
      let meta = (mkBangumi "Frieren" (Just 2023) Nothing) {group = [GroupName "喵萌奶茶屋", GroupName "LoliHouse"]}
          file = BangumiFile meta (Episode 1) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01 [喵萌奶茶屋] [LoliHouse].mkv",
    testCase "NCOP without index" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (NCOP Nothing Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/extras/NCOP.mkv",
    testCase "NCOP with index" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (NCOP (Just 2) Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/extras/NCOP2.mkv",
    testCase "NCED without index" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (NCED Nothing Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/extras/NCED.mkv",
    testCase "NCED with index" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (NCED (Just 3) Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/extras/NCED3.mkv",
    testCase "NCED with index and episode" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (NCED (Just 2) (Just 47)) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/extras/NCED2_EP47.mkv",
    testCase "Menu without volume" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Menu Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/extras/Menu.mkv",
    testCase "Menu with volume" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Menu (Just 1)) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/extras/Menu1.mkv",
    testCase "PV trailer" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (PV 1) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/trailers/PV1.mkv",
    testCase "Preview without episode" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Preview Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/trailers/Preview.mkv",
    testCase "Preview with episode" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Preview (Just 27)) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/trailers/Preview_EP27.mkv",
    testCase "Trailer" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Trailer Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/trailers/Trailer.mkv",
    testCase "CM without index" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (CM Nothing) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/trailers/CM.mkv",
    testCase "CM with index" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (CM (Just 2)) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/trailers/CM2.mkv",
    testCase "movie" $ do
      let meta = mkBangumi "Frieren Movie" Nothing Nothing
          file = BangumiFile meta (Movie 2025) "mkv"
      generateFullPath file @?= "Frieren Movie (2025)/Frieren Movie (2025).mkv",
    testCase "subtitle file CHS" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (EpisodeSub 1 CHS) "ass"
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.chs.ass",
    testCase "subtitle file JPN" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (EpisodeSub 1 JPN) "srt"
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.jpn.srt",
    testCase "no year in meta" $ do
      let meta = mkBangumi "Frieren" Nothing Nothing
          file = BangumiFile meta (Episode 1) "mkv"
      generateFullPath file @?= "Frieren/Season 01/Frieren - S01E01.mkv",
    testCase "with TMDB ID" $ do
      let meta = mkBangumi "Frieren" (Just 2023) (Just 12345)
          file = BangumiFile meta (Episode 1) "mkv"
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.mkv",
    testCase "MP4 extension" $ do
      let meta = mkBangumi "Frieren" (Just 2023) Nothing
          file = BangumiFile meta (Episode 1) "mp4"
      generateFullPath file @?= "Frieren (2023)/Season 01/Frieren - S01E01.mp4"
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
      let meta = mkBangumi "Re:Zero" (Just 2016) Nothing
          file = BangumiFile meta (Episode 1) "mkv"
      generateFullPath file @?= "Re-Zero (2016)/Season 01/Re-Zero - S01E01.mkv"
  ]
