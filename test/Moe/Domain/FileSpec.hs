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
      (BangumiFile frieren (mkContent (Episode 1)) "mkv")
      "Frieren/Season 01/Frieren - S01E01.mkv",
    checkPath "regular episode S02E15"
      (BangumiFile frieren {season = Just (SeasonIndex 2)} (mkContent (Episode 15)) "mkv")
      "Frieren/Season 02/Frieren - S02E15.mkv",
    checkPath "special episode S00E01"
      (BangumiFile frieren (mkContent (Special 1)) "mkv")
      "Frieren/Season 00/Frieren - S00E01.mkv",
    checkPath "special subtitle S00E01 CHS"
      (BangumiFile frieren (mkContentSub (Special 1) CHS) "ass")
      "Frieren/Season 00/Frieren - S00E01.zh-Hans.ass",
    checkPath "episode with group"
      (BangumiFile frieren {group = [GroupName "SweetSub"]} (mkContent (Episode 1)) "mkv")
      "Frieren/Season 01/Frieren - S01E01 [SweetSub].mkv",
    checkPath "episode with multiple groups"
      (BangumiFile frieren {group = [GroupName "喵萌奶茶屋", GroupName "LoliHouse"]} (mkContent (Episode 1)) "mkv")
      "Frieren/Season 01/Frieren - S01E01 [喵萌奶茶屋] [LoliHouse].mkv",
    checkPath "NCOP without index"
      (BangumiFile frieren (mkContent (NCOP Nothing Nothing)) "mkv")
      "Frieren/Season 01/Featurettes/NCOP.mkv",
    checkPath "NCOP with index"
      (BangumiFile frieren (mkContent (NCOP (Just 2) Nothing)) "mkv")
      "Frieren/Season 01/Featurettes/NCOP2.mkv",
    checkPath "NCED without index"
      (BangumiFile frieren (mkContent (NCED Nothing Nothing)) "mkv")
      "Frieren/Season 01/Featurettes/NCED.mkv",
    checkPath "NCED with index"
      (BangumiFile frieren (mkContent (NCED (Just 3) Nothing)) "mkv")
      "Frieren/Season 01/Featurettes/NCED3.mkv",
    checkPath "NCED with index and episode"
      (BangumiFile frieren (mkContent (NCED (Just 2) (Just 47))) "mkv")
      "Frieren/Season 01/Featurettes/NCED2_EP47.mkv",
    checkPath "Menu without volume"
      (BangumiFile frieren (mkContent (Menu Nothing)) "mkv")
      "Frieren/Season 01/Other/Menu.mkv",
    checkPath "Menu with volume"
      (BangumiFile frieren (mkContent (Menu (Just 1))) "mkv")
      "Frieren/Season 01/Other/Menu1.mkv",
    checkPath "PV trailer"
      (BangumiFile frieren (mkContent (PV 1)) "mkv")
      "Frieren/Season 01/Trailers/PV1.mkv",
    checkPath "Preview without episode"
      (BangumiFile frieren (mkContent (Preview Nothing)) "mkv")
      "Frieren/Season 01/Trailers/Preview.mkv",
    checkPath "Preview with episode"
      (BangumiFile frieren (mkContent (Preview (Just 27))) "mkv")
      "Frieren/Season 01/Trailers/Preview_EP27.mkv",
    checkPath "Trailer"
      (BangumiFile frieren (mkContent (Trailer Nothing)) "mkv")
      "Frieren/Season 01/Trailers/Trailer.mkv",
    checkPath "CM without index"
      (BangumiFile frieren (mkContent (CM Nothing)) "mkv")
      "Frieren/Season 01/Other/CM.mkv",
    checkPath "CM with index"
      (BangumiFile frieren (mkContent (CM (Just 2))) "mkv")
      "Frieren/Season 01/Other/CM2.mkv",
    checkPath "movie"
      (BangumiFile (BangumiMeta "Frieren Movie" Nothing Nothing [] False) (mkContent Movie) "mkv")
      "Frieren Movie/Frieren Movie.mkv",
    checkPath "subtitle file CHS"
      (BangumiFile frieren (mkContentSub (Episode 1) CHS) "ass")
      "Frieren/Season 01/Frieren - S01E01.zh-Hans.ass",
    checkPath "subtitle file JPN"
      (BangumiFile frieren (mkContentSub (Episode 1) JPN) "srt")
      "Frieren/Season 01/Frieren - S01E01.jpn.srt",
    checkPath "subtitle file JPSC mapped to CHS"
      (BangumiFile frieren (mkContentSub (Episode 1) CHS) "ass")
      "Frieren/Season 01/Frieren - S01E01.zh-Hans.ass",
    checkPath "with TMDB ID"
      (BangumiFile frieren {tmdbId = Just 12345} (mkContent (Episode 1)) "mkv")
      "Frieren/Season 01/Frieren - S01E01.mkv",
    checkPath "special chars in title"
      (BangumiFile (BangumiMeta "Re:Zero" Nothing (Just (SeasonIndex 1)) [] False) (mkContent (Episode 1)) "mkv")
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
