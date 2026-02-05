module Moe.App.Subscription.Washing
  ( WashingResult (..),
    parseRawItem,
    processWashing,
    buildEpisodeMap,
  )
where

import Data.List (findIndex)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Moe.Domain.Bangumi.Episode (Episode (..), EpisodeNumber (..))
import Moe.Domain.Bangumi.Internal.Group (Group (..), GroupName (..))
import Moe.Domain.Bangumi.Parser.RssTitle (RssTitleInfo (..), parseRssTitle)
import Moe.Domain.Bangumi.Types (Bangumi, BangumiF (..))
import Moe.Domain.Setting.Types (WashingConfig (..))
import Moe.Infrastructure.Rss.Types (RawItem (..))
import Moe.Prelude

data WashingResult
  = NewEpisode Episode
  | UpgradeEpisode Episode Episode
  | SkipEpisode
  deriving stock (Show, Eq)

buildEpisodeMap :: [Episode] -> Map EpisodeNumber Episode
buildEpisodeMap = Map.fromList . map (\e -> (e.episodeNumber, e))

-- | Parse a raw item: validate required fields and parse RSS title into an Episode
parseRawItem :: Bangumi -> RawItem -> Maybe Episode
parseRawItem bangumi item = do
  title <- item.title
  torrentUrl <- item.torrentUrl
  infoHash <- item.infoHash
  pubDate <- item.pubDate
  let parsed = parseRssTitle title
  epNum <- parsed.episode
  pure
    Episode
      { id = Nothing,
        bangumiId = bangumi.id,
        episodeNumber = epNum,
        group = parsed.group,
        resolution = parsed.resolution,
        infoHash = infoHash,
        torrentUrl = torrentUrl,
        pubDate = pubDate,
        createdAt = Nothing
      }

-- | Decide washing result for a parsed episode against existing episodes
processWashing ::
  Map EpisodeNumber Episode ->
  Maybe WashingConfig ->
  Episode ->
  WashingResult
processWashing episodeMap mConfig newEp =
  case Map.lookup newEp.episodeNumber episodeMap of
    Nothing -> NewEpisode newEp
    Just existingEp ->
      let groups = maybe [] (.groupPriority) mConfig
       in if shouldUpgrade groups existingEp.group newEp.group
            then UpgradeEpisode newEp existingEp
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
