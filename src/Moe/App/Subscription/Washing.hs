module Moe.App.Subscription.Washing
  ( WashingResult (..),
    parseFilteredItem,
    processWashing,
    buildEpisodeMap,
  )
where

import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Moe.App.Subscription.Types (DownloadTask (..), FilteredItem (..), ParsedItem (..))
import Moe.Domain.Bangumi.Episode (Episode (..), EpisodeNumber (..))
import Moe.Domain.Bangumi.Internal.Group (Group (..), GroupName (..))
import Moe.Domain.Bangumi.Parser.RssTitle (RssTitleInfo (..), parseRssTitle)
import Moe.Domain.Bangumi.Types (Bangumi (..))
import Moe.Domain.Setting.Types (WashingConfig (..))
import Moe.Infrastructure.Rss.Types (RawItem (..))
import Moe.Prelude

data WashingResult
  = NewEpisode DownloadTask Episode
  | UpgradeEpisode DownloadTask Episode Episode
  | SkipEpisode
  deriving stock (Show, Eq)

buildEpisodeMap :: [Episode] -> Map EpisodeNumber Episode
buildEpisodeMap = Map.fromList . map (\e -> (e.episodeNumber, e))

-- | Parse a filtered item: validate required fields and parse RSS title
parseFilteredItem :: FilteredItem -> Maybe ParsedItem
parseFilteredItem fi = do
  title <- fi.item.title
  torrentUrl <- fi.item.torrentUrl
  infoHash <- fi.item.infoHash
  bangumiId <- fi.bangumi.id
  let parsed = parseRssTitle title
  epNum <- parsed.episode
  pure
    ParsedItem
      { bangumi = fi.bangumi,
        bangumiId = bangumiId,
        torrentUrl = torrentUrl,
        infoHash = infoHash,
        pubDate = fi.parsedPubDate,
        episodeNumber = epNum,
        group = parsed.group,
        resolution = parsed.resolution
      }

-- | Decide washing result for a parsed item against existing episodes
processWashing ::
  Map EpisodeNumber Episode ->
  Maybe WashingConfig ->
  ParsedItem ->
  WashingResult
processWashing episodeMap mConfig pi_ =
  let newEpisode =
        Episode
          { id = Nothing,
            bangumiId = pi_.bangumiId,
            episodeNumber = pi_.episodeNumber,
            group = pi_.group,
            resolution = pi_.resolution,
            infoHash = pi_.infoHash,
            torrentUrl = pi_.torrentUrl,
            pubDate = pi_.pubDate,
            createdAt = Nothing
          }

      task =
        DownloadTask
          { bangumi = pi_.bangumi,
            torrentUrl = pi_.torrentUrl,
            infoHash = Just pi_.infoHash,
            pubDate = pi_.pubDate
          }
   in case Map.lookup pi_.episodeNumber episodeMap of
        Nothing -> NewEpisode task newEpisode
        Just existingEp ->
          let groups = maybe [] (.groupPriority) mConfig
           in if shouldUpgrade groups existingEp.group pi_.group
                then UpgradeEpisode task newEpisode existingEp
                else SkipEpisode

shouldUpgrade :: [Group] -> Maybe GroupName -> Maybe GroupName -> Bool
shouldUpgrade _ Nothing (Just _) = True
shouldUpgrade _ _ Nothing = False
shouldUpgrade groups (Just existing) (Just new)
  | existing == new = False
  | otherwise =
      let existingIdx = findGroupIndex groups existing
          newIdx = findGroupIndex groups new
       in case (existingIdx, newIdx) of
            (Nothing, Just _) -> True
            (Just ei, Just ni) -> ni < ei
            _ -> False

-- | Find the index of a group name in the priority list, considering aliases
findGroupIndex :: [Group] -> GroupName -> Maybe Int
findGroupIndex groups gn =
  findIndex (matchesGroup gn) groups
  where
    matchesGroup name g =
      T.toLower (toText name) == T.toLower (toText g.name)
        || any (\a -> T.toLower (toText name) == T.toLower a) g.aliases
