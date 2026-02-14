module Moe.Job.Rename.Strategy.CollectionSpec (tests) where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (runErrorWith)
import Effectful.Log (LogLevel (..))
import Moe.App.Logging (makeLogger, runLog)
import Moe.Domain.File (GroupName (..))
import Moe.Error (AppError)
import Moe.Domain.Setting (UserPreference (..), TMDBConfig (..), defaultUserPreference, defaultTMDBConfig)
import Moe.Infra.Metadata.Effect (Metadata, runMetadataHttp)
import Moe.Infra.Setting.Effect (Setting (..))
import Moe.Job.Rename.Strategy.Collection (processFile)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Moe.Prelude
import System.Directory (doesFileExist)
import System.Environment (setEnv)
import Network.HTTP.Client.TLS (newTlsManager)
import Test.Tasty
import Test.Tasty.HUnit

-- | Mock Setting that injects TMDB API key from environment.
runTestSetting :: Text -> Eff (Setting : es) a -> Eff es a
runTestSetting apiKey = interpret $ \_ -> \case
  GetSetting -> pure defaultUserPreference {tmdb = defaultTMDBConfig {apiKey}}

-- | Run an action with real HTTP metadata + test Setting/Log.
runIntegrationEffects :: Text -> Eff '[Metadata, Error AppError, Setting, Log, Concurrent, IOE] a -> IO a
runIntegrationEffects apiKey action = do
  manager <- newTlsManager
  runEff
    $ withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    $ runConcurrent
    $ makeLogger "/tmp/moe-bangumi-test"
    $ \logger ->
      runLog "test" logger LogInfo
        $ runTestSetting apiKey
        $ runErrorWith (\_ err -> error $ "Unexpected error: " <> show err)
        $ runMetadataHttp manager action

-- | Tests

tests :: TestTree
tests =
  testGroup
    "Moe.Job.Rename.Strategy.Collection"
    [ testGroup "processFile" processFileTests
    ]

-- | Load TMDB API key from .env file or environment variable. Skip test if not available.
withTmdbApiKey :: (Text -> Assertion) -> Assertion
withTmdbApiKey f = do
  loadDotenv
  mKey <- lookupEnv "TMDB_API_KEY"
  case mKey of
    Nothing -> pure ()
    Just key -> f (toText key)

-- | Load .env file into process environment if it exists.
loadDotenv :: IO ()
loadDotenv = do
  exists <- doesFileExist ".env"
  when exists $ do
    content <- TIO.readFile ".env"
    forM_ (T.lines content) $ \line ->
      case T.breakOn "=" (T.strip line) of
        (key, val)
          | not (T.null key), not (T.null val), not (T.isPrefixOf "#" key) ->
              setEnv (toString key) (toString $ T.drop 1 val)
        _ -> pure ()

-- | (label, bdrip, groups, input, expected)
type TestCase = (String, Bool, [GroupName], Text, Maybe Text)

multiRoot :: Text
multiRoot = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu"

season2 :: Text
season2 = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [Ma10p_1080p]"

season3 :: Text
season3 = "[hyakuhuyu&VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 3rd Season [Ma10p_1080p]"

movie :: Text
movie = "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Hyouketsu no Kizuna [Ma10p_1080p]"

testCases :: [TestCase]
testCases =
  [ ( "Episode",
      True,
      [GroupName "VCB-Studio"],
      multiRoot <> "/" <> season2
        <> "/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [26][Ma10p_1080p][x265_flac_aac].mkv",
      Just "Re：从零开始的异世界生活 (2016)/Season 02/Re：从零开始的异世界生活 - S02E26 [VCB-Studio][BDRip].mkv"
    ),
    ( "CD",
      True,
      [GroupName "VCB-Studio"],
      multiRoot <> "/" <> season2
        <> "/CDs/[200826] ｢Realize｣／鈴木このみ (flac+webp)/Scans/02.webp",
      Just "Re：从零开始的异世界生活 (2016)/Season 02/CDs/[200826] ｢Realize｣／鈴木このみ (flac+webp)/Scans/02.webp"
    ),
    ( "CM in SPs",
      True,
      [GroupName "VCB-Studio"],
      multiRoot <> "/" <> season2
        <> "/Sps/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [CM01][Ma10p_1080p][x265_flac].mkv",
      Just "Re：从零开始的异世界生活 (2016)/Season 02/Other/CM1.mkv"
    ),
    ( "SP",
      True,
      [GroupName "VCB-Studio"],
      multiRoot <> "/" <> season2
        <> "/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 2nd Season [SP03_02][Ma10p_1080p][x265_flac].mkv",
      Just "Re：从零开始的异世界生活 (2016)/Season 00/Re：从零开始的异世界生活 - S00E302 [VCB-Studio][BDRip].mkv"
    ),
    ( "Movie",
      True,
      [GroupName "VCB-Studio"],
      multiRoot <> "/" <> movie
        <> "/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Hyouketsu no Kizuna [Ma10p_1080p][x265_flac].mka",
      Just "Re：从零开始的异世界生活 冰结之绊 (2019)/Re：从零开始的异世界生活 冰结之绊 (2019) [VCB-Studio][BDRip].mka"
    ),
    ( "Movie 2",
      True,
      [GroupName "VCB-Studio"],
      multiRoot <> "/" <> "[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Memory Snow [Ma10p_1080p]"
        <> "/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Memory Snow [Ma10p_1080p][x265_flac_aac].mkv",
      Just "Re：从零开始的异世界生活 雪之回忆 (2018)/Re：从零开始的异世界生活 雪之回忆 (2018) [VCB-Studio][BDRip].mkv"
    ),
    ( "Movie CM",
      True,
      [GroupName "VCB-Studio"],
      multiRoot <> "/" <> movie
        <> "/[VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu Hyouketsu no Kizuna [CM01][Ma10p_1080p][x265_flac].mkv",
      Just "Re：从零开始的异世界生活 冰结之绊 (2019)/Other/CM1.mkv"
    ),
    ( "Subtitle",
      True,
      [GroupName "VCB-Studio"],
      season3
        <> "/[hyakuhuyu&VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 3rd Season [51][Ma10p_1080p][x265_flac_aac].CHS.ass",
      Just "Re：从零开始的异世界生活 (2016)/Season 03/Re：从零开始的异世界生活 - S03E51 [VCB-Studio][BDRip].zh-Hans.ass"
    ),
    ( "CD under torrent root",
      True,
      [GroupName "VCB-Studio"],
      season3
        <> "/CDs/[200826] ｢Realize｣／鈴木このみ (flac+webp)/02.webp",
      Just "Re：从零开始的异世界生活 (2016)/Season 03/CDs/[200826] ｢Realize｣／鈴木このみ (flac+webp)/02.webp"
    ),
    ( "Mini Anime in SPs",
      True,
      [GroupName "VCB-Studio"],
      season3
        <> "/SPs/[hyakuhuyu&VCB-Studio] Re Zero kara Hajimeru Isekai Seikatsu 3rd Season [Mini Anime 60][Ma10p_1080p][x265_flac].CHT.ass",
      Just "Re：从零开始的异世界生活 (2016)/Season 00/Re：从零开始的异世界生活 - S00E60 [VCB-Studio][BDRip].zh-Hant.ass"
    )
  ]

processFileTests :: [TestTree]
processFileTests =
  [ testCase "processFile" $ withTmdbApiKey $ \apiKey ->
      runIntegrationEffects apiKey $ do
        void $ foldM (\bmap (label, bdrip, grp, input, expected) -> do
          (bmap', result) <- processFile bdrip grp bmap input
          liftIO $ assertEqual label expected result
          pure bmap'
          ) Map.empty testCases
  ]
