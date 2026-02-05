module Moe.App.Subscription.Washing
  ( parseRawItem,
    processWashing,
    buildEpisodeMap,
  )
where

import Data.List (findIndex, minimum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Moe.Domain.Bangumi.Episode (Episode (..), EpisodeNumber (..))
import Moe.Domain.Bangumi.Internal.Group (Group (..), GroupName (..))
import Moe.Domain.Bangumi.Internal.Subtitle (SubtitleList, defaultSubtitlePriority)
import Moe.Domain.Bangumi.Parser.RssTitle (RssTitleInfo (..), parseRssTitle)
import Moe.Domain.Bangumi.Types (Bangumi, BangumiF (..))
import Moe.Domain.Setting.Types (WashingConfig (..))
import Moe.Infrastructure.Rss.Types (RawItem (..))
import Moe.Prelude

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
        subtitleList = parsed.subtitleList,
        resolution = parsed.resolution,
        infoHash = infoHash,
        torrentUrl = torrentUrl,
        pubDate = pubDate,
        createdAt = Nothing
      }

-- | Process washing for a list of parsed episodes against existing episodes.
--
-- Returns (toAdd, toDelete):
-- - toAdd: episodes to download (new episodes + upgraded new versions)
-- - toDelete: old episodes to remove (replaced by upgrades)
processWashing ::
  Map EpisodeNumber Episode ->
  Maybe WashingConfig ->
  [Episode] ->
  ([Episode], [Episode])
processWashing episodeMap mConfig = foldr go ([], [])
  where
    groups = maybe [] (.groupPriority) mConfig
    subPrios = maybe defaultSubtitlePriority (.subtitlePriority) mConfig
    go newEp (adds, deletes) =
      case Map.lookup newEp.episodeNumber episodeMap of
        Nothing -> (newEp : adds, deletes)
        Just existingEp
          | shouldUpgrade groups subPrios existingEp newEp ->
              (newEp : adds, existingEp : deletes)
          | otherwise -> (adds, deletes)

-- | Check if new episode should upgrade existing episode.
-- Group priority is primary, subtitle priority is secondary tiebreaker.
shouldUpgrade :: [Group] -> [SubtitleList] -> Episode -> Episode -> Bool
shouldUpgrade groups subPrios existing new =
  case compareGroupPriority groups existing.group new.group of
    LT -> True
    GT -> False
    EQ -> compareSubtitlePriority subPrios existing.subtitleList new.subtitleList == LT

-- | Compare group priority. LT means new is better.
compareGroupPriority :: [Group] -> [GroupName] -> [GroupName] -> Ordering
compareGroupPriority _ [] [] = EQ
compareGroupPriority _ [] (_:_) = LT
compareGroupPriority _ _ [] = GT
compareGroupPriority groups existing new
  | existing == new = EQ
  | otherwise =
      let existingBest = bestGroupIndex groups existing
          newBest = bestGroupIndex groups new
       in case (existingBest, newBest) of
            (Nothing, Just _) -> LT
            (Just ei, Just ni) -> compare ni ei
            (Just _, Nothing) -> GT
            (Nothing, Nothing) -> EQ

-- | Compare subtitle priority. LT means new is better.
compareSubtitlePriority :: [SubtitleList] -> SubtitleList -> SubtitleList -> Ordering
compareSubtitlePriority _ existing new
  | sort existing == sort new = EQ
compareSubtitlePriority prios existing new =
  let existingIdx = findSubtitleIndex prios existing
      newIdx = findSubtitleIndex prios new
   in case (existingIdx, newIdx) of
        (Nothing, Just _) -> LT
        (Just ei, Just ni) -> compare ni ei
        (Just _, Nothing) -> GT
        (Nothing, Nothing) -> EQ

-- | Find the index of a subtitle list in the priority list (set comparison)
findSubtitleIndex :: [SubtitleList] -> SubtitleList -> Maybe Int
findSubtitleIndex prios subs =
  let sorted = sort subs
   in findIndex (\p -> sort p == sorted) prios

-- | Find the best (lowest) priority index among a list of group names
bestGroupIndex :: [Group] -> [GroupName] -> Maybe Int
bestGroupIndex groups gns =
  let indices = mapMaybe (findGroupIndex groups) gns
   in if null indices then Nothing else Just (minimum indices)

-- | Find the index of a group name in the priority list, considering aliases
findGroupIndex :: [Group] -> GroupName -> Maybe Int
findGroupIndex groups gn =
  findIndex (matchesGroup gn) groups
  where
    matchesGroup name g =
      T.toLower (toText name) == T.toLower (toText g.name)
        || any (\a -> T.toLower (toText name) == T.toLower a) g.aliases
