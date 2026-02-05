module Moe.App.Subscription.WashingSpec (tests) where

import Data.Text qualified as T
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Moe.App.Subscription.Filter (filterItems)
import Moe.App.Subscription.Types (RssContext (..))
import Moe.App.Subscription.Washing (buildEpisodeMap, parseRawItem, processWashing)
import Moe.Domain.Bangumi.Episode (Episode (..), EpisodeNumber (..))
import Moe.Domain.Bangumi.Internal.Group (Group (..), GroupName (..))
import Moe.Domain.Bangumi.Types (Bangumi, BangumiF (..), BangumiId (..), BangumiKind (..))
import Moe.Domain.Rss.Types (PubDate (..))
import Moe.Domain.Setting.Types (WashingConfig (..), defaultFilterConfig)
import Moe.Infrastructure.Rss.Types (RawItem (..))
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Moe.App.Subscription.Washing"
    [ testGroup "Medalist RSS (mikanani.me/3519)" medalistTests
    ]

-- | Test bangumi: Medalist (金牌得主)
testBangumi :: Bangumi
testBangumi =
  Bangumi
    { id = BangumiId 1,
      titleChs = "金牌得主",
      titleJap = Just "メダリスト",
      airDate = Just (fromGregorian 2025 1 5),
      season = Nothing,
      kind = Tv,
      mikanId = Nothing,
      tmdbId = Nothing,
      bgmtvId = Nothing,
      posterUrl = Nothing,
      createdAt = UTCTime (fromGregorian 2025 1 1) 0
    }

testContext :: RssContext
testContext =
  RssContext
    { bangumi = testBangumi,
      rssUrl = "https://mikanani.me/RSS/Bangumi?bangumiId=3519",
      lastPubdate = Nothing
    }

mkPubDate :: Int -> Int -> Int -> Int -> Int -> Maybe PubDate
mkPubDate y m d hh mm =
  Just $ PubDate $ UTCTime (fromGregorian (fromIntegral y) m d) (fromIntegral $ hh * 3600 + mm * 60)

-- | All RSS items from mikanani.me/RSS/Bangumi?bangumiId=3519
medalistRssItems :: [RawItem]
medalistRssItems =
  [ RawItem (Just "[7\xB3\&ACG] 金牌得主/メダリスト/Medalist S01 | 01-13 [简繁字幕] BDrip 1080p x265 OPUS 2.0") (mkPubDate 2025 6 10 12 10) (Just "https://mikanani.me/Download/20250610/90b8e954575d0959df8536e443fbbbf4466e8964.torrent") (Just "90b8e954575d0959df8536e443fbbbf4466e8964"),
    RawItem (Just "[7\xB3\&ACG] 金牌得主/メダリスト/Medalist S01 | 01-13 [简繁字幕] BDrip 1080p AV1 OPUS 2.0") (mkPubDate 2025 6 6 11 26) (Just "https://mikanani.me/Download/20250606/bcb1c98b9a013dbb5b135c29c90d88b1fd99ec70.torrent") (Just "bcb1c98b9a013dbb5b135c29c90d88b1fd99ec70"),
    RawItem (Just "[Prejudice-Studio] 金牌得主 Medalist [01-13][Bilibili WEB-DL 1080P AVC 8bit AVC MP4][简日内嵌]") (mkPubDate 2025 6 4 19 25) (Just "https://mikanani.me/Download/20250604/b13e00e0202866a3f0cda26addb8d4b4546fdd33.torrent") (Just "b13e00e0202866a3f0cda26addb8d4b4546fdd33"),
    RawItem (Just "六四位元字幕组★金牌得主 Medalist★01~12★1920x1080★BDRIP AVC FLAC MKV★外挂繁体中文(采用D+字幕)[组庆三年]") (mkPubDate 2025 6 3 22 38) (Just "https://mikanani.me/Download/20250603/1186ad2ef9ab71603392a7e3891d3914be69cc34.torrent") (Just "1186ad2ef9ab71603392a7e3891d3914be69cc34"),
    RawItem (Just "[DBD-Raws][金牌得主/Medalist][01-13TV全集][1080P][BDRip][HEVC-10bit][简繁日双语外挂][FLAC][MKV](メダリスト)") (mkPubDate 2025 5 29 19 51) (Just "https://mikanani.me/Download/20250529/7d1fc6bb588cfe19f03c171f4cb40af5e2957fd0.torrent") (Just "7d1fc6bb588cfe19f03c171f4cb40af5e2957fd0"),
    RawItem (Just "[DBD-Raws][金牌得主/Medalist][06-09TV][BOX2][1080P][BDRip][HEVC-10bit][FLAC][MKV](メダリスト)") (mkPubDate 2025 4 26 1 58) (Just "https://mikanani.me/Download/20250426/0fce56728f3c9ef4c362c58609a505970b85d55a.torrent") (Just "0fce56728f3c9ef4c362c58609a505970b85d55a"),
    RawItem (Just "[黒ネズミたち] 金牌得主 / Medalist - 13 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 4 4 1 31) (Just "https://mikanani.me/Download/20250404/442684dea8f3ec33d30f22653a6240be1e1bd3a5.torrent") (Just "442684dea8f3ec33d30f22653a6240be1e1bd3a5"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 13 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 3 30 22 25) (Just "https://mikanani.me/Download/20250330/c9ef9cb58fd959e961dba8d9c3efde08597ed59f.torrent") (Just "c9ef9cb58fd959e961dba8d9c3efde08597ed59f"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][13][1080p][繁日双语][招募翻译]") (mkPubDate 2025 3 30 20 58) (Just "https://mikanani.me/Download/20250330/634babd1496008762a93c43e88c57677fb92f57a.torrent") (Just "634babd1496008762a93c43e88c57677fb92f57a"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][13][1080p][简日双语][招募翻译]") (mkPubDate 2025 3 30 20 58) (Just "https://mikanani.me/Download/20250330/ba9f76d59dd6c05d44ab6c9205a619dc1bf94408.torrent") (Just "ba9f76d59dd6c05d44ab6c9205a619dc1bf94408"),
    RawItem (Just "[TOC] 金牌得主 1-13 [1080P][AVC AAC][CHT][MP4] 第2期制作决定") (mkPubDate 2025 3 30 1 11) (Just "https://mikanani.me/Download/20250330/d3bffd4f356f3ef86dfd0cc2429e32e3da871187.torrent") (Just "d3bffd4f356f3ef86dfd0cc2429e32e3da871187"),
    RawItem (Just "メダリスト (JPBD Vol.1 Remux) 金牌得主 舞冰的祈愿 Medalist") (mkPubDate 2025 3 29 1 58) (Just "https://mikanani.me/Download/20250329/20a680b86182e3fc9f4bfd7b8b2444029e46c7b8.torrent") (Just "20a680b86182e3fc9f4bfd7b8b2444029e46c7b8"),
    RawItem (Just "[DBD-Raws][金牌得主/Medalist][01-05TV][BOX1][1080P][BDRip][HEVC-10bit][FLAC][MKV](メダリスト)") (mkPubDate 2025 3 28 14 37) (Just "https://mikanani.me/Download/20250328/d0f82a01517bcf0fd3b771ade1318d118a0320dd.torrent") (Just "d0f82a01517bcf0fd3b771ade1318d118a0320dd"),
    RawItem (Just "[黒ネズミたち] 金牌得主 / Medalist - 12 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 3 28 1 31) (Just "https://mikanani.me/Download/20250328/e882d90d2860a25754716554f2114f0fb4fd3eae.torrent") (Just "e882d90d2860a25754716554f2114f0fb4fd3eae"),
    RawItem (Just "[TOC] 金牌得主 12 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 3 24 12 13) (Just "https://mikanani.me/Download/20250324/8ee78c113b914a2b6148c36ee775a93f65d81abb.torrent") (Just "8ee78c113b914a2b6148c36ee775a93f65d81abb"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 12 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 3 24 7 22) (Just "https://mikanani.me/Download/20250324/851c58ae80cfeaee21ed64cdcaf12ab377a8dac4.torrent") (Just "851c58ae80cfeaee21ed64cdcaf12ab377a8dac4"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][12][1080p][繁日双语][招募翻译]") (mkPubDate 2025 3 24 0 53) (Just "https://mikanani.me/Download/20250324/d4d1d5d48e24e314f9178c65a0d1c63f4185b52f.torrent") (Just "d4d1d5d48e24e314f9178c65a0d1c63f4185b52f"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][12][1080p][简日双语][招募翻译]") (mkPubDate 2025 3 24 0 53) (Just "https://mikanani.me/Download/20250324/e04302eca46da0b58539670166d9a8d663f546b6.torrent") (Just "e04302eca46da0b58539670166d9a8d663f546b6"),
    RawItem (Just "[黒ネズミたち] 金牌得主 / Medalist - 11 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 3 21 1 31) (Just "https://mikanani.me/Download/20250321/fbfa498e2ac1e5ddcf4e2860be632f14f71b811d.torrent") (Just "fbfa498e2ac1e5ddcf4e2860be632f14f71b811d"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 11 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 3 17 7 2) (Just "https://mikanani.me/Download/20250317/80bddc361e10bb3dfa965c4c2ebf04b5e9c173fc.torrent") (Just "80bddc361e10bb3dfa965c4c2ebf04b5e9c173fc"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][11][1080p][简日双语][招募翻译]") (mkPubDate 2025 3 17 0 4) (Just "https://mikanani.me/Download/20250317/dd831f958ce5986ff55cea3b55b56a2afcecc13c.torrent") (Just "dd831f958ce5986ff55cea3b55b56a2afcecc13c"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][11][1080p][繁日双语][招募翻译]") (mkPubDate 2025 3 17 0 4) (Just "https://mikanani.me/Download/20250317/82ac4c166cad030f3a7231c1ac812245a927ae5a.torrent") (Just "82ac4c166cad030f3a7231c1ac812245a927ae5a"),
    RawItem (Just "[TOC] 金牌得主 11 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 3 16 1 16) (Just "https://mikanani.me/Download/20250316/fd7ae6b1c7276868e69b88ab7b404f7e8f0a8482.torrent") (Just "fd7ae6b1c7276868e69b88ab7b404f7e8f0a8482"),
    RawItem (Just "[黒ネズミたち] 金牌得主 / Medalist - 10 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 3 14 1 30) (Just "https://mikanani.me/Download/20250314/634157483c1ef90fb1394bb45a1cb1235990471a.torrent") (Just "634157483c1ef90fb1394bb45a1cb1235990471a"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 10 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 3 10 18 38) (Just "https://mikanani.me/Download/20250310/507c39201b45dc64b8f9a27fc8cc87731551aebf.torrent") (Just "507c39201b45dc64b8f9a27fc8cc87731551aebf"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][10][1080p][繁日双语][招募翻译]") (mkPubDate 2025 3 10 10 29) (Just "https://mikanani.me/Download/20250310/22d418c9de25f53bb719749d00f7c87c5d98c70d.torrent") (Just "22d418c9de25f53bb719749d00f7c87c5d98c70d"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][10][1080p][简日双语][招募翻译]") (mkPubDate 2025 3 10 10 28) (Just "https://mikanani.me/Download/20250310/b5bbba133e02739cbb3be8aff1a9d6b46c3b7c3a.torrent") (Just "b5bbba133e02739cbb3be8aff1a9d6b46c3b7c3a"),
    RawItem (Just "[TOC] 金牌得主 10 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 3 9 1 44) (Just "https://mikanani.me/Download/20250309/a54b8633ce72456d7cbb317f692102be5a1a5c7b.torrent") (Just "a54b8633ce72456d7cbb317f692102be5a1a5c7b"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 09 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 3 7 1 30) (Just "https://mikanani.me/Download/20250307/2e425e63eb4de7e2b87e6e43ee89d418eedd85b1.torrent") (Just "2e425e63eb4de7e2b87e6e43ee89d418eedd85b1"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 09 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 3 3 0 3) (Just "https://mikanani.me/Download/20250303/f6a8a4cd86b616c37693f87af2f12def5f7f6c15.torrent") (Just "f6a8a4cd86b616c37693f87af2f12def5f7f6c15"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][09][1080p][繁日双语][招募翻译]") (mkPubDate 2025 3 2 23 56) (Just "https://mikanani.me/Download/20250302/eb24571b4692d58af33dc13d145861bb99a1a960.torrent") (Just "eb24571b4692d58af33dc13d145861bb99a1a960"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][09][1080p][简日双语][招募翻译]") (mkPubDate 2025 3 2 23 56) (Just "https://mikanani.me/Download/20250302/a3f975138986e310b5db13411ec9ccead87cae22.torrent") (Just "a3f975138986e310b5db13411ec9ccead87cae22"),
    RawItem (Just "[TOC] 金牌得主 09 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 3 2 1 14) (Just "https://mikanani.me/Download/20250302/22537eb6e2f4bcd9071b317d12a9e9daecae364a.torrent") (Just "22537eb6e2f4bcd9071b317d12a9e9daecae364a"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 08 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 2 28 1 30) (Just "https://mikanani.me/Download/20250228/3530946d1c73b52bab7491f0ea508750193dc508.torrent") (Just "3530946d1c73b52bab7491f0ea508750193dc508"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 08 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 2 23 20 10) (Just "https://mikanani.me/Download/20250223/06dac4ce6236c3327bf1cf44608ddf50e68026d5.torrent") (Just "06dac4ce6236c3327bf1cf44608ddf50e68026d5"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][08][1080p][繁日双语][招募翻译]") (mkPubDate 2025 2 23 18 41) (Just "https://mikanani.me/Download/20250223/4e39e7605840f966510dc1d9c10aeaa31d85f1ec.torrent") (Just "4e39e7605840f966510dc1d9c10aeaa31d85f1ec"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][08][1080p][简日双语][招募翻译]") (mkPubDate 2025 2 23 18 40) (Just "https://mikanani.me/Download/20250223/4c8bf5b55f86a5affbd1d8f8f37b6c72f8e63cf1.torrent") (Just "4c8bf5b55f86a5affbd1d8f8f37b6c72f8e63cf1"),
    RawItem (Just "[TOC] 金牌得主 08 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 2 23 1 41) (Just "https://mikanani.me/Download/20250223/318848ce33d94f31ff77478ee310e99a5c7b422b.torrent") (Just "318848ce33d94f31ff77478ee310e99a5c7b422b"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 07 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 2 21 1 31) (Just "https://mikanani.me/Download/20250221/5ac93f2eda4f3072598d92da593fe46d060f35ab.torrent") (Just "5ac93f2eda4f3072598d92da593fe46d060f35ab"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 07 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 2 16 23 4) (Just "https://mikanani.me/Download/20250216/8af97f629574c0d9d4db7525d138fd7a6b6a9905.torrent") (Just "8af97f629574c0d9d4db7525d138fd7a6b6a9905"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][07][1080p][繁日双语][招募翻译]") (mkPubDate 2025 2 16 22 50) (Just "https://mikanani.me/Download/20250216/27da900acbc535042c1e98d6a7821f656a3b9dcd.torrent") (Just "27da900acbc535042c1e98d6a7821f656a3b9dcd"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][07][1080p][简日双语][招募翻译]") (mkPubDate 2025 2 16 22 50) (Just "https://mikanani.me/Download/20250216/9a2c995a817d6811808ba04152ed56a65f2076f4.torrent") (Just "9a2c995a817d6811808ba04152ed56a65f2076f4"),
    RawItem (Just "[TOC] 金牌得主 07 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 2 16 1 28) (Just "https://mikanani.me/Download/20250216/506226ecebd228b6bf91085b4f85a6c486dfe75f.torrent") (Just "506226ecebd228b6bf91085b4f85a6c486dfe75f"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 06 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 2 10 7 25) (Just "https://mikanani.me/Download/20250210/21b784aa56898157f3bd152bf17aeb81094e6b00.torrent") (Just "21b784aa56898157f3bd152bf17aeb81094e6b00"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][06][1080p][繁日双语][招募翻译]") (mkPubDate 2025 2 9 23 45) (Just "https://mikanani.me/Download/20250209/d656a77d643c5155b1358ca398caaa8fde2b9393.torrent") (Just "d656a77d643c5155b1358ca398caaa8fde2b9393"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][06][1080p][简日双语][招募翻译]") (mkPubDate 2025 2 9 23 45) (Just "https://mikanani.me/Download/20250209/930f849dce1179c8e48b10f7ac52122378d15e62.torrent") (Just "930f849dce1179c8e48b10f7ac52122378d15e62"),
    RawItem (Just "[TOC] 金牌得主 06 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 2 9 1 31) (Just "https://mikanani.me/Download/20250209/56e4f02e19b4544bcfa50668da6f6c03cadd43ef.torrent") (Just "56e4f02e19b4544bcfa50668da6f6c03cadd43ef"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 05 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 2 7 1 31) (Just "https://mikanani.me/Download/20250207/3589af4dc46d8a8e0e3654cbd642f1966861dd90.torrent") (Just "3589af4dc46d8a8e0e3654cbd642f1966861dd90"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 05 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 2 3 1 27) (Just "https://mikanani.me/Download/20250203/3c45b65d861002a341315a02381efdb876c834bc.torrent") (Just "3c45b65d861002a341315a02381efdb876c834bc"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][05][1080p][繁日双语][招募翻译]") (mkPubDate 2025 2 2 22 31) (Just "https://mikanani.me/Download/20250202/5119a23ec8594100a00a0abb246527aee6b4d7b5.torrent") (Just "5119a23ec8594100a00a0abb246527aee6b4d7b5"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][05][1080p][简日双语][招募翻译]") (mkPubDate 2025 2 2 22 31) (Just "https://mikanani.me/Download/20250202/886b64159740a9aee728bc221c97bb37846898e3.torrent") (Just "886b64159740a9aee728bc221c97bb37846898e3"),
    RawItem (Just "[TOC] 金牌得主 05 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 2 2 1 32) (Just "https://mikanani.me/Download/20250202/6a3c1aca604ccc4f27e9708a4d9980dd42d76223.torrent") (Just "6a3c1aca604ccc4f27e9708a4d9980dd42d76223"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 04 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 1 31 1 30) (Just "https://mikanani.me/Download/20250131/958658a4fc41bfb1cee623a0656890b03788c141.torrent") (Just "958658a4fc41bfb1cee623a0656890b03788c141"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 04 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 1 26 23 4) (Just "https://mikanani.me/Download/20250126/88c33896af1ef1e5affbe4f57880bcaa92607bc8.torrent") (Just "88c33896af1ef1e5affbe4f57880bcaa92607bc8"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][04][1080p][繁日双语][招募翻译]") (mkPubDate 2025 1 26 22 53) (Just "https://mikanani.me/Download/20250126/3433d7db6f2ff5f07f123cbcb82e6ca52f21cab6.torrent") (Just "3433d7db6f2ff5f07f123cbcb82e6ca52f21cab6"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][04][1080p][简日双语][招募翻译]") (mkPubDate 2025 1 26 22 52) (Just "https://mikanani.me/Download/20250126/ca01469cce119949141f2c176e4703b0c800f312.torrent") (Just "ca01469cce119949141f2c176e4703b0c800f312"),
    RawItem (Just "[TOC] 金牌得主 04 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 1 26 1 15) (Just "https://mikanani.me/Download/20250126/0c0654bfcbf803acc6f111e7eeecb8921d7919dd.torrent") (Just "0c0654bfcbf803acc6f111e7eeecb8921d7919dd"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 03 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 1 24 1 30) (Just "https://mikanani.me/Download/20250124/2958b51a563ea1598b3bf7add8d868b686f83c36.torrent") (Just "2958b51a563ea1598b3bf7add8d868b686f83c36"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 03 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 1 20 0 21) (Just "https://mikanani.me/Download/20250120/95db42bd56e11040f8615327acb04fc56112b053.torrent") (Just "95db42bd56e11040f8615327acb04fc56112b053"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][03][1080p][繁日双语][招募翻译]") (mkPubDate 2025 1 20 0 13) (Just "https://mikanani.me/Download/20250120/14b93b4027c8c03a0a7eacbd4b35717e6e4765d8.torrent") (Just "14b93b4027c8c03a0a7eacbd4b35717e6e4765d8"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][03][1080p][简日双语][招募翻译]") (mkPubDate 2025 1 20 0 13) (Just "https://mikanani.me/Download/20250120/7cb366ef59b4ed7dee952178ed0c9512ebf0e7b2.torrent") (Just "7cb366ef59b4ed7dee952178ed0c9512ebf0e7b2"),
    RawItem (Just "[TOC] 金牌得主 03 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 1 19 1 33) (Just "https://mikanani.me/Download/20250119/91b648ce4f6f35bda1fe6a3b2cf57ba8fef4808f.torrent") (Just "91b648ce4f6f35bda1fe6a3b2cf57ba8fef4808f"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 02 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 1 17 1 30) (Just "https://mikanani.me/Download/20250117/50f95c2b0bd746fc764ce26acda13ae0967c8baa.torrent") (Just "50f95c2b0bd746fc764ce26acda13ae0967c8baa"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 02 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 1 13 11 42) (Just "https://mikanani.me/Download/20250113/99ba9b3155c7dd6c8692692069acdef175d11860.torrent") (Just "99ba9b3155c7dd6c8692692069acdef175d11860"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][02][1080p][繁日双语][招募翻译]") (mkPubDate 2025 1 13 2 35) (Just "https://mikanani.me/Download/20250113/6e7309c7713f605453b3e1a96f45b2872fa92ea2.torrent") (Just "6e7309c7713f605453b3e1a96f45b2872fa92ea2"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][02][1080p][简日双语][招募翻译]") (mkPubDate 2025 1 13 2 34) (Just "https://mikanani.me/Download/20250113/e003159787bb95120186c5d7a6c29deac6790b3d.torrent") (Just "e003159787bb95120186c5d7a6c29deac6790b3d"),
    RawItem (Just "[TOC] 金牌得主 02 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 1 12 1 30) (Just "https://mikanani.me/Download/20250112/7d417961bc5e4e7ac45d3ec373dbcbdada5c3b51.torrent") (Just "7d417961bc5e4e7ac45d3ec373dbcbdada5c3b51"),
    RawItem (Just "[Up to 21\xB0\&C] 金牌得主 / Medalist - 01 (ABEMA 1920x1080 AVC AAC MP4)") (mkPubDate 2025 1 10 7 59) (Just "https://mikanani.me/Download/20250110/af37f2287bf5ae3635d02d26346cd75652307dbd.torrent") (Just "af37f2287bf5ae3635d02d26346cd75652307dbd"),
    RawItem (Just "[喵萌奶茶屋&LoliHouse] 金牌得主 / Medalist - 01 [WebRip 1080p HEVC-10bit AAC][简繁日内封字幕]") (mkPubDate 2025 1 5 22 57) (Just "https://mikanani.me/Download/20250105/08312e4d95d91c0eafd9886401241447914ed075.torrent") (Just "08312e4d95d91c0eafd9886401241447914ed075"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][01][1080p][繁日双语][招募翻译]") (mkPubDate 2025 1 5 21 58) (Just "https://mikanani.me/Download/20250105/c5e254cafa93fa51a726784422809a94c98f71b7.torrent") (Just "c5e254cafa93fa51a726784422809a94c98f71b7"),
    RawItem (Just "【喵萌奶茶屋】★01月新番★[金牌得主 / Medalist][01][1080p][简日双语][招募翻译]") (mkPubDate 2025 1 5 21 58) (Just "https://mikanani.me/Download/20250105/3b9b3ffa89fa725e01fc39aff4bbd5036ac01703.torrent") (Just "3b9b3ffa89fa725e01fc39aff4bbd5036ac01703"),
    RawItem (Just "[TOC] 金牌得主 01 [1080P][AVC AAC][CHT][MP4]") (mkPubDate 2025 1 5 8 6) (Just "https://mikanani.me/Download/20250105/5dc38e66e43d08003205f3ee0848baffc326f538.torrent") (Just "5dc38e66e43d08003205f3ee0848baffc326f538")
  ]

expectedGroup :: [GroupName]
expectedGroup = [GroupName "喵萌奶茶屋", GroupName "LoliHouse"]

medalistTests :: [TestTree]
medalistTests =
  [ testCase "filter removes batch releases (\\d-\\d pattern)" $ do
      let filtered = filterItems (Just defaultFilterConfig) testContext medalistRssItems
          titles = mapMaybe (.title) filtered
          -- Batch releases like "01-13", "06-09", "01-05", "01~12", "1-13" should be filtered
          hasBatch = any (\t -> "01-13" `T.isInfixOf` t || "06-09" `T.isInfixOf` t || "01-05" `T.isInfixOf` t) titles
      hasBatch @?= False,
    testCase "filter + parse produces episodes 01-13 from multiple groups" $ do
      let filtered = filterItems (Just defaultFilterConfig) testContext medalistRssItems
          parsed = mapMaybe (parseRawItem testBangumi) filtered
          epNums = sort . ordNub $ map (.episodeNumber) parsed
      -- After filtering batches, individual episodes 1-13 remain from multiple groups
      epNums @?= map EpisodeNumber [1 .. 13],
    testCase "parse extracts 喵萌奶茶屋&LoliHouse group correctly (split by &)" $ do
      let filtered = filterItems (Just defaultFilterConfig) testContext medalistRssItems
          parsed = mapMaybe (parseRawItem testBangumi) filtered
          loliHouseEps = filter (\ep -> ep.group == expectedGroup) parsed
          loliHouseEpNums = sort $ map (.episodeNumber) loliHouseEps
      -- 喵萌奶茶屋&LoliHouse splits into [喵萌奶茶屋, LoliHouse]
      loliHouseEpNums @?= map EpisodeNumber [1 .. 13],
    testCase "wash with priority [LoliHouse] upgrades from solo 喵萌奶茶屋 group" $ do
      let washingConfig =
            WashingConfig
              { groupPriority =
                  [Group (GroupName "LoliHouse") []]
              }
          filtered = filterItems (Just defaultFilterConfig) testContext medalistRssItems
          parsed = mapMaybe (parseRawItem testBangumi) filtered

          -- Simulate: all 13 episodes already exist with solo 喵萌奶茶屋 group
          existingEps =
            [ Episode
                { id = Nothing,
                  bangumiId = BangumiId 1,
                  episodeNumber = EpisodeNumber n,
                  group = [GroupName "喵萌奶茶屋"],
                  resolution = Just "1080P",
                  infoHash = "existing_hash_" <> show n,
                  torrentUrl = "existing_url",
                  pubDate = PubDate (UTCTime (fromGregorian 2025 1 1) 0),
                  createdAt = Nothing
                }
              | n <- [1 .. 13]
            ]
          episodeMap = buildEpisodeMap existingEps
          (toAdd, toDelete) = processWashing episodeMap (Just washingConfig) parsed

      -- 喵萌奶茶屋&LoliHouse splits into [喵萌奶茶屋, LoliHouse]
      -- LoliHouse is in priority list, so these episodes match and upgrade existing solo 喵萌奶茶屋
      let addEpNums = sort . ordNub $ map (.episodeNumber) toAdd
      addEpNums @?= map EpisodeNumber [1 .. 13]

      -- All 13 existing episodes should be in toDelete
      let deleteEpNums = sort $ map (.episodeNumber) toDelete
      deleteEpNums @?= map EpisodeNumber [1 .. 13]

      -- Verify toAdd contains the split [喵萌奶茶屋, LoliHouse] group
      let loliHouseAdds = filter (\ep -> ep.group == expectedGroup) toAdd
      length loliHouseAdds @?= 13
  ]
