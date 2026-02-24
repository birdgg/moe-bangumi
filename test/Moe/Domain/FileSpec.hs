module Moe.Domain.FileSpec (tests) where

import Moe.Domain.File
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

-- | Default BangumiMeta for most tests.
frieren :: BangumiMeta
frieren = BangumiMeta
  { title = "Frieren", tmdbId = Nothing, season = Just (SeasonIndex 1), group = [], isBDrip = False }

-- | Assert generateFullPath produces expected path.
checkPath :: String -> BangumiFile -> FilePath -> TestTree
checkPath label file expected = testCase label $ generateFullPath file @?= expected

tests :: TestTree
tests =
  testGroup
    "Moe.Domain.File"
    [ testGroup "generateFullPath" namingTests,
      testGroup "sanitizeName" sanitizationTests
    ]

namingTests :: [TestTree]
namingTests =
  [ checkPath "regular episode S01E01"
      (BangumiFile frieren (Episode 1) "mkv")
      "Frieren/Season 01/Frieren - S01E01.mkv",
    checkPath "regular episode S02E15"
      (BangumiFile frieren {season = Just (SeasonIndex 2)} (Episode 15) "mkv")
      "Frieren/Season 02/Frieren - S02E15.mkv",
    checkPath "special episode S00E01"
      (BangumiFile frieren (Special 1) "mkv")
      "Frieren/Season 00/Frieren - S00E01.mkv",
    checkPath "special subtitle S00E01 CHS"
      (BangumiFile frieren (SpecialSub 1 CHS) "ass")
      "Frieren/Season 00/Frieren - S00E01.zh-Hans.ass",
    checkPath "episode with group"
      (BangumiFile frieren {group = [GroupName "SweetSub"]} (Episode 1) "mkv")
      "Frieren/Season 01/Frieren - S01E01 [SweetSub].mkv",
    checkPath "episode with multiple groups"
      (BangumiFile frieren {group = [GroupName "喵萌奶茶屋", GroupName "LoliHouse"]} (Episode 1) "mkv")
      "Frieren/Season 01/Frieren - S01E01 [喵萌奶茶屋] [LoliHouse].mkv",
    checkPath "NCOP without index"
      (BangumiFile frieren (NCOP Nothing Nothing) "mkv")
      "Frieren/Season 01/Featurettes/NCOP.mkv",
    checkPath "NCOP with index"
      (BangumiFile frieren (NCOP (Just 2) Nothing) "mkv")
      "Frieren/Season 01/Featurettes/NCOP2.mkv",
    checkPath "NCED without index"
      (BangumiFile frieren (NCED Nothing Nothing) "mkv")
      "Frieren/Season 01/Featurettes/NCED.mkv",
    checkPath "NCED with index"
      (BangumiFile frieren (NCED (Just 3) Nothing) "mkv")
      "Frieren/Season 01/Featurettes/NCED3.mkv",
    checkPath "NCED with index and episode"
      (BangumiFile frieren (NCED (Just 2) (Just 47)) "mkv")
      "Frieren/Season 01/Featurettes/NCED2_EP47.mkv",
    checkPath "Menu without volume"
      (BangumiFile frieren (Menu Nothing) "mkv")
      "Frieren/Season 01/Other/Menu.mkv",
    checkPath "Menu with volume"
      (BangumiFile frieren (Menu (Just 1)) "mkv")
      "Frieren/Season 01/Other/Menu1.mkv",
    checkPath "PV trailer"
      (BangumiFile frieren (PV 1) "mkv")
      "Frieren/Season 01/Trailers/PV1.mkv",
    checkPath "Preview without episode"
      (BangumiFile frieren (Preview Nothing) "mkv")
      "Frieren/Season 01/Trailers/Preview.mkv",
    checkPath "Preview with episode"
      (BangumiFile frieren (Preview (Just 27)) "mkv")
      "Frieren/Season 01/Trailers/Preview_EP27.mkv",
    checkPath "Trailer"
      (BangumiFile frieren (Trailer Nothing) "mkv")
      "Frieren/Season 01/Trailers/Trailer.mkv",
    checkPath "CM without index"
      (BangumiFile frieren (CM Nothing) "mkv")
      "Frieren/Season 01/Other/CM.mkv",
    checkPath "CM with index"
      (BangumiFile frieren (CM (Just 2)) "mkv")
      "Frieren/Season 01/Other/CM2.mkv",
    checkPath "movie"
      (BangumiFile (BangumiMeta "Frieren Movie" Nothing Nothing [] False) Movie "mkv")
      "Frieren Movie/Frieren Movie.mkv",
    checkPath "subtitle file CHS"
      (BangumiFile frieren (EpisodeSub 1 CHS) "ass")
      "Frieren/Season 01/Frieren - S01E01.zh-Hans.ass",
    checkPath "subtitle file JPN"
      (BangumiFile frieren (EpisodeSub 1 JPN) "srt")
      "Frieren/Season 01/Frieren - S01E01.jpn.srt",
    checkPath "with TMDB ID"
      (BangumiFile frieren {tmdbId = Just 12345} (Episode 1) "mkv")
      "Frieren/Season 01/Frieren - S01E01.mkv",
    checkPath "special chars in title"
      (BangumiFile (BangumiMeta "Re:Zero" Nothing (Just (SeasonIndex 1)) [] False) (Episode 1) "mkv")
      "Re-Zero/Season 01/Re-Zero - S01E01.mkv"
  ]

sanitizationTests :: [TestTree]
sanitizationTests =
  [ testCase "colon replaced with dash" $ sanitizeName "Re:Zero" @?= "Re-Zero",
    testCase "forward slash replaced with dash" $ sanitizeName "Fate/Zero" @?= "Fate-Zero",
    testCase "backslash replaced with dash" $ sanitizeName "Test\\Name" @?= "Test-Name",
    testCase "forbidden chars removed" $ sanitizeName "Test<>|?*\"Name" @?= "TestName",
    testCase "mixed special chars" $ sanitizeName "Re:Zero / Fate<Test>" @?= "Re-Zero - FateTest",
    testCase "no changes needed" $ sanitizeName "Normal Name" @?= "Normal Name"
  ]
