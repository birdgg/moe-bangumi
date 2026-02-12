-- | Washing logic for RSS subscription episode upgrades.
--
-- Determines which new episodes should be downloaded and which existing
-- episodes should be replaced based on group and subtitle priority.
module Moe.Job.Subscription.Washing
  ( parseRawItem,
    processWashing,
    buildEpisodeMap,
  )
where

import Data.List (elemIndex, findIndex)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Moe.Domain.Episode (Episode (..), EpisodeNumber (..))
import Moe.Domain.Shared.Group (Group (..), GroupName (..))
import Moe.Domain.Shared.Subtitle (SubtitleList)
import Moe.Domain.Parser.RssTitle (RssTitleInfo (..), parseRssTitle)
import Moe.Domain.Bangumi (Bangumi)
import Moe.Domain.Setting (WashingConfig (..))
import Moe.Domain.Shared.Entity (Entity (..))
import Moe.Infra.Rss.Types (RawItem (..))
import Moe.Prelude

-- | Build a map from episode number to entity for lookup during washing.
-- Uses Map.fromList which keeps last entry for duplicate keys;
-- acceptable since DB returns unique episode numbers per bangumi.
buildEpisodeMap :: [Entity Episode] -> Map EpisodeNumber (Entity Episode)
buildEpisodeMap = Map.fromList . map (\e -> (e.entityVal.episodeNumber, e))

-- | Parse a raw item: validate required fields and parse RSS title into an Episode.
parseRawItem :: [Group] -> Entity Bangumi -> RawItem -> Maybe Episode
parseRawItem groups bangumi item = do
  title <- item.title
  torrentUrl <- item.torrentUrl
  infoHash <- item.infoHash
  pubDate <- item.pubDate
  let parsed = parseRssTitle groups title
  epNum <- parsed.episode
  pure
    Episode
      { bangumiId = bangumi.entityId,
        episodeNumber = epNum,
        group = parsed.group,
        subtitleList = parsed.subtitleList,
        resolution = parsed.resolution,
        infoHash = infoHash,
        torrentUrl = torrentUrl,
        pubDate = pubDate
      }

-- | Process washing for a list of parsed episodes against existing episodes.
--
-- Returns (toAdd, toDelete):
-- - toAdd: episodes to download (new episodes + upgraded new versions)
-- - toDelete: old episodes to remove (replaced by upgrades)
--
-- For new episodes (not in DB), picks the best group per episode number.
-- For existing episodes, upgrades only when group/subtitle priority is better.
processWashing ::
  Map EpisodeNumber (Entity Episode) ->
  WashingConfig ->
  [Episode] ->
  ([Episode], [Entity Episode])
processWashing episodeMap config eps =
  let (newMap, deleteMap) = foldr go (Map.empty, Map.empty) eps
   in (Map.elems newMap, Map.elems deleteMap)
  where
    groups = config.groupPriority
    subPrios = config.subtitlePriority
    go newEp (adds, dels) =
      case Map.lookup newEp.episodeNumber episodeMap of
        Nothing ->
          -- No existing DB episode: pick the best among new candidates
          let adds' = Map.insertWith (pickBetter groups subPrios) newEp.episodeNumber newEp adds
           in (adds', dels)
        Just existingEp
          | shouldUpgrade groups subPrios existingEp newEp ->
              let adds' = Map.insertWith (pickBetter groups subPrios) newEp.episodeNumber newEp adds
                  dels' = Map.insert newEp.episodeNumber existingEp dels
               in (adds', dels')
          | otherwise -> (adds, dels)

-- | Keep the higher-priority episode between two candidates.
pickBetter :: [Group] -> [SubtitleList] -> Episode -> Episode -> Episode
pickBetter groups subPrios new old =
  case compareGroupPriority groups old.group new.group of
    LT -> new
    GT -> old
    EQ ->
      case compareSubtitlePriority subPrios old.subtitleList new.subtitleList of
        LT -> new
        _ -> old

-- | Check if new episode should upgrade existing episode.
-- Group priority is primary, subtitle priority is secondary tiebreaker.
shouldUpgrade :: [Group] -> [SubtitleList] -> Entity Episode -> Episode -> Bool
shouldUpgrade groups subPrios existing new =
  case compareGroupPriority groups existing.entityVal.group new.group of
    LT -> True
    GT -> False
    EQ -> compareSubtitlePriority subPrios existing.entityVal.subtitleList new.subtitleList == LT

-- | Compare group priority. LT means new is better.
compareGroupPriority :: [Group] -> [GroupName] -> [GroupName] -> Ordering
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
-- Pre-sorts inputs once and uses pre-sorted priority list to avoid repeated sorting.
compareSubtitlePriority :: [SubtitleList] -> SubtitleList -> SubtitleList -> Ordering
compareSubtitlePriority prios existing new =
  let sortedExisting = sort existing
      sortedNew = sort new
   in if sortedExisting == sortedNew
        then EQ
        else
          let sortedPrios = map sort prios
              existingIdx = elemIndex sortedExisting sortedPrios
              newIdx = elemIndex sortedNew sortedPrios
           in case (existingIdx, newIdx) of
                (Nothing, Just _) -> LT
                (Just ei, Just ni) -> compare ni ei
                (Just _, Nothing) -> GT
                (Nothing, Nothing) -> EQ

-- | Find the best (lowest) priority index among a list of group names.
bestGroupIndex :: [Group] -> [GroupName] -> Maybe Int
bestGroupIndex groups gns =
  case mapMaybe (findGroupIndex groups) gns of
    [] -> Nothing
    (x : xs) -> Just (foldl' min x xs)

-- | Find the index of a group name in the priority list, considering aliases
findGroupIndex :: [Group] -> GroupName -> Maybe Int
findGroupIndex groups gn =
  findIndex (matchesGroup gn) groups
  where
    matchesGroup name g =
      let nameLower = T.toLower (toText name)
       in nameLower == T.toLower (toText g.name)
            || any (\a -> nameLower == T.toLower a) g.aliases
