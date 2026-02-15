module Moe.Job.Subscription.WashingSpec (tests) where

import Data.Text qualified as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Moe.Domain.Bangumi (Bangumi (..), BangumiKind (..))
import Moe.Domain.Episode (Episode (..), EpisodeNumber (..))
import Moe.Domain.Rss (PubDate (..))
import Moe.Domain.Setting (WashingConfig (..), defaultFilterConfig, defaultGroupPriority)
import Moe.Domain.Shared.Entity (Entity (..), Id (..))
import Moe.Domain.Shared.Group (Group (..), GroupName (..))
import Moe.Domain.Shared.Subtitle (Subtitle (..))
import Moe.Infra.Rss.Types (RawItem (..))
import Moe.Job.Subscription (RssContext (..), filterItems)
import Moe.Job.Subscription.Washing (buildEpisodeMap, parseRawItem, processWashing)
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Moe.Job.Subscription.Washing"
    [ testGroup "filter & parse" filterParseTests,
      testGroup "processWashing" washingTests
    ]

-- -------------------------------------------------------------------
-- Shared helpers
-- -------------------------------------------------------------------

dummyPubDate :: PubDate
dummyPubDate = PubDate (UTCTime (fromGregorian 2025 1 1) 0)

mkEp :: Word32 -> [GroupName] -> [Subtitle] -> Episode
mkEp n grp subs =
  Episode
    { bangumiId = Id 1,
      episodeNumber = EpisodeNumber n,
      group = grp,
      subtitleList = subs,
      resolution = Just "1080P",
      infoHash = "hash_" <> show n <> "_" <> show grp,
      torrentUrl = "url",
      pubDate = dummyPubDate
    }

mkExistingEp :: Int64 -> Word32 -> [GroupName] -> [Subtitle] -> Entity Episode
mkExistingEp eid n grp subs =
  Entity
    { entityId = Id eid,
      entityVal = mkEp n grp subs,
      createdAt = UTCTime (fromGregorian 2025 1 1) 0,
      updatedAt = UTCTime (fromGregorian 2025 1 1) 0
    }

loliHouseGroup :: [GroupName]
loliHouseGroup = [GroupName "喵萌奶茶屋", GroupName "LoliHouse"]

soloNyaaGroup :: [GroupName]
soloNyaaGroup = [GroupName "喵萌奶茶屋"]

tocGroup :: [GroupName]
tocGroup = [GroupName "TOC"]

loliHouseConfig :: WashingConfig
loliHouseConfig =
  WashingConfig
    { groupPriority = [Group (GroupName "LoliHouse") []],
      subtitlePriority = []
    }

-- -------------------------------------------------------------------
-- Filter & parse tests (minimal RSS items)
-- -------------------------------------------------------------------

testBangumi :: Entity Bangumi
testBangumi =
  Entity
    { entityId = Id 1,
      entityVal =
        Bangumi
          { titleChs = "金牌得主",
            titleJap = Just "メダリスト",
            airDate = fromGregorian 2025 1 5,
            firstAirYear = Nothing,
            season = Nothing,
            kind = Tv,
            mikanId = Nothing,
            tmdbId = Nothing,
            bgmtvId = Nothing,
            posterUrl = Nothing,
            totalEpisodes = Nothing
          },
      createdAt = UTCTime (fromGregorian 2025 1 1) 0,
      updatedAt = UTCTime (fromGregorian 2025 1 1) 0
    }

testContext :: RssContext
testContext =
  RssContext
    { bangumi = testBangumi,
      rssUrl = "https://mikanani.me/RSS/Bangumi?bangumiId=3519",
      lastPubdate = Nothing,
      autoComplete = True,
      episodeOffset = 0
    }

mkPubDate :: Int -> Int -> Int -> Int -> Int -> Maybe PubDate
mkPubDate y m d hh mm =
  Just $ PubDate $ UTCTime (fromGregorian (fromIntegral y) m d) (fromIntegral $ hh * 3600 + mm * 60)

-- | Minimal RSS items covering: batch release, joint group, solo group, unknown group
minimalRssItems :: [RawItem]
minimalRssItems =
  [ -- Batch (should be filtered by \d-\d)
    RawItem (Just "[DBD-Raws][金牌得主/Medalist][01-13TV全集][1080P][BDRip][HEVC-10bit][FLAC][MKV]") (mkPubDate 2025 5 29 19 51) (Just "url1") (Just "hash1"),
    -- Joint group: 喵萌奶茶屋&LoliHouse ep 01
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 01 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 1 5 22 57) (Just "url2") (Just "hash2"),
    -- Solo group: 喵萌奶茶屋 ep 01
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][01][1080p][简日双语][招募翻译]") (mkPubDate 2025 1 5 21 58) (Just "url3") (Just "hash3"),
    -- Unknown group: TOC ep 01
    RawItem (Just "[TOC] 金牌得主 01 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 1 5 8 6) (Just "url4") (Just "hash4")
  ]

filterParseTests :: [TestTree]
filterParseTests =
  [ testCase "filter removes batch releases" $ do
      let filtered = filterItems defaultFilterConfig testContext minimalRssItems
          titles = mapMaybe (.title) filtered
          hasBatch = any (\t -> "01-13" `T.isInfixOf` t) titles
      hasBatch @?= False
      length filtered @?= 3,
    testCase "parse extracts joint group split by &" $ do
      let filtered = filterItems defaultFilterConfig testContext minimalRssItems
          parsed = mapMaybe (parseRawItem defaultGroupPriority testBangumi) filtered
          jointEps = filter (\ep -> ep.group == loliHouseGroup) parsed
      length jointEps @?= 1
      map (.episodeNumber) jointEps @?= [EpisodeNumber 1]
  ]

-- -------------------------------------------------------------------
-- Washing tests (directly constructed Episodes)
-- -------------------------------------------------------------------

washingTests :: [TestTree]
washingTests =
  [ testCase "upgrade: replaces lower-priority group with higher-priority" $ do
      let existing = [mkExistingEp 1 1 soloNyaaGroup []]
          episodeMap = buildEpisodeMap existing
          newEps = [mkEp 1 loliHouseGroup []]
          (toAdd, toDelete) = processWashing episodeMap loliHouseConfig newEps

      map (.episodeNumber) toAdd @?= [EpisodeNumber 1]
      map (.group) toAdd @?= [loliHouseGroup]
      map (\e -> e.entityVal.episodeNumber) toDelete @?= [EpisodeNumber 1],
    testCase "no upgrade: same group is not re-added" $ do
      let existing = [mkExistingEp 1 1 loliHouseGroup []]
          episodeMap = buildEpisodeMap existing
          newEps = [mkEp 1 loliHouseGroup []]
          (toAdd, toDelete) = processWashing episodeMap loliHouseConfig newEps

      toAdd @?= []
      toDelete @?= [],
    testCase "no upgrade: unknown group does not replace priority group" $ do
      let existing = [mkExistingEp 1 1 loliHouseGroup []]
          episodeMap = buildEpisodeMap existing
          newEps = [mkEp 1 tocGroup []]
          (toAdd, toDelete) = processWashing episodeMap loliHouseConfig newEps

      toAdd @?= []
      toDelete @?= [],
    testCase "empty DB: picks best group per episode (no duplicates)" $ do
      let episodeMap = buildEpisodeMap []
          -- 3 versions of ep 1 from different groups
          newEps =
            [ mkEp 1 loliHouseGroup [],
              mkEp 1 soloNyaaGroup [],
              mkEp 1 tocGroup []
            ]
          (toAdd, toDelete) = processWashing episodeMap loliHouseConfig newEps

      length toAdd @?= 1
      map (.group) toAdd @?= [loliHouseGroup]
      toDelete @?= [],
    testCase "empty DB: multiple episodes each deduplicated" $ do
      let episodeMap = buildEpisodeMap []
          newEps =
            [ mkEp 1 loliHouseGroup [],
              mkEp 1 tocGroup [],
              mkEp 2 tocGroup [],
              mkEp 2 loliHouseGroup [],
              mkEp 3 soloNyaaGroup []
            ]
          (toAdd, toDelete) = processWashing episodeMap loliHouseConfig newEps
          addMap = sortOn fst $ map (\ep -> (ep.episodeNumber, ep.group)) toAdd

      length toAdd @?= 3
      addMap
        @?= [ (EpisodeNumber 1, loliHouseGroup),
              (EpisodeNumber 2, loliHouseGroup),
              (EpisodeNumber 3, soloNyaaGroup)
            ]
      toDelete @?= [],
    testCase "upgrade: multiple new eps for same existing produce no duplicate deletes" $ do
      let existing = [mkExistingEp 1 1 tocGroup []]
          episodeMap = buildEpisodeMap existing
          -- Two new eps for same episode, both upgrade the existing
          newEps =
            [ mkEp 1 loliHouseGroup [],
              mkEp 1 soloNyaaGroup []
            ]
          (toAdd, toDelete) = processWashing episodeMap loliHouseConfig newEps

      length toAdd @?= 1
      map (.group) toAdd @?= [loliHouseGroup]
      -- Should only have one delete entry, not two
      length toDelete @?= 1,
    testCase "empty group treated same as unknown group" $ do
      let config =
            WashingConfig
              { groupPriority = [Group (GroupName "LoliHouse") []],
                subtitlePriority = []
              }
          episodeMap = buildEpisodeMap []
          newEps =
            [ mkEp 1 [] [],
              mkEp 1 loliHouseGroup []
            ]
          (toAdd, _) = processWashing episodeMap config newEps

      length toAdd @?= 1
      map (.group) toAdd @?= [loliHouseGroup],
    testCase "subtitle tiebreaker: upgrades when group equal but subtitle better" $ do
      let config =
            WashingConfig
              { groupPriority = [Group (GroupName "LoliHouse") []],
                subtitlePriority = [[CHS, JPN], [CHS]]
              }
          existing = [mkExistingEp 1 1 loliHouseGroup [CHS]]
          episodeMap = buildEpisodeMap existing
          newEps = [mkEp 1 loliHouseGroup [CHS, JPN]]
          (toAdd, toDelete) = processWashing episodeMap config newEps

      length toAdd @?= 1
      map (.subtitleList) toAdd @?= [[CHS, JPN]]
      length toDelete @?= 1
  ]
