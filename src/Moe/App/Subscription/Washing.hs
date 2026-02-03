module Moe.App.Subscription.Washing
  ( WashingResult (..),
    processWashing,
    buildEpisodeMap,
  )
where

import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Moe.App.Subscription.Types (DownloadTask (..), FilteredItem (..))
import Moe.Domain.Bangumi.Parser.RssTitle (RssTitleInfo (..), parseRssTitle)
import Moe.Domain.Bangumi.Episode.Types (Episode (..), EpisodeNumber (..))
import Moe.Domain.Setting.Types (FilterConfig (..))
import Moe.Infrastructure.Rss.Types (RawItem (..))

data WashingResult
  = NewEpisode DownloadTask Episode
  | UpgradeEpisode DownloadTask Episode Episode
  | SkipEpisode
  deriving stock (Show, Eq)

buildEpisodeMap :: [Episode] -> Map EpisodeNumber Episode
buildEpisodeMap = Map.fromList . map (\e -> (e.episodeNumber, e))

processWashing ::
  Map EpisodeNumber Episode ->
  Maybe FilterConfig ->
  FilteredItem ->
  Maybe WashingResult
processWashing episodeMap mConfig fi = do
  title <- fi.item.title
  torrentUrl <- fi.item.torrentUrl
  infoHash <- fi.item.infoHash

  let parsed = parseRssTitle title
  epNum <- parsed.episode

  let newEpisode =
        Episode
          { id = Nothing,
            bangumiId = fi.bangumiId,
            episodeNumber = epNum,
            subtitleGroup = parsed.group,
            resolution = parsed.resolution,
            infoHash = infoHash,
            torrentUrl = torrentUrl,
            pubDate = fi.parsedPubDate,
            createdAt = Nothing
          }

  let task =
        DownloadTask
          { bangumiId = fi.bangumiId,
            torrentUrl = torrentUrl,
            infoHash = Just infoHash,
            pubDate = fi.parsedPubDate
          }

  case Map.lookup epNum episodeMap of
    Nothing -> Just $ NewEpisode task newEpisode
    Just existingEp ->
      let priority = maybe [] (.subtitleGroupPriority) mConfig
       in if shouldUpgrade priority existingEp.subtitleGroup parsed.group
            then Just $ UpgradeEpisode task newEpisode existingEp
            else Just SkipEpisode

shouldUpgrade :: [Text] -> Maybe Text -> Maybe Text -> Bool
shouldUpgrade _ Nothing (Just _) = True
shouldUpgrade _ _ Nothing = False
shouldUpgrade priority (Just existing) (Just new)
  | existing == new = False
  | otherwise =
      let existingIdx = elemIndex existing priority
          newIdx = elemIndex new priority
       in case (existingIdx, newIdx) of
            (Nothing, Just _) -> True
            (Just ei, Just ni) -> ni < ei
            _ -> False
