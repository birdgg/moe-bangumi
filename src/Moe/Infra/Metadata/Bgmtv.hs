module Moe.Infra.Metadata.Bgmtv
  ( mkBgmtvClient,
    buildBgmtvRequest,
    bgmtvSubjectToBangumi,
    bgmtvDetailToBangumi,
    platformToKind,
  )
where

import Data.Time.Calendar (Day)
import Moe.Domain.Bangumi (Bangumi (..), BangumiKind (..), BgmtvId (..))
import Moe.Domain.Parser (ParsedTitle (..), parseOriginalTitle)
import Moe.Prelude
import Network.HTTP.Client (Manager)
import Web.Bgmtv.Client qualified as Bgmtv
import Web.Bgmtv.Types.Id (SubjectId (..))
import Web.Bgmtv.Types.Subject (Subject (..), SubjectDetail (..), SubjectImages (..))
import Web.Bgmtv.Types.Enums qualified as Bgmtv
import Web.Bgmtv.Types.Search qualified as Bgmtv

bgmtvUserAgent :: Text
bgmtvUserAgent = "moe-bangumi/0.1.0"

-- | Create a Bgmtv API client.
mkBgmtvClient :: Manager -> Bgmtv.BgmtvClient
mkBgmtvClient = Bgmtv.newBgmtvClientWith ?? Bgmtv.defaultConfig bgmtvUserAgent

-- | Build an anime search request.
buildBgmtvRequest :: Text -> Bgmtv.SearchRequest
buildBgmtvRequest keyword =
  Bgmtv.SearchRequest
    { keyword = keyword,
      filter_ =
        Just
          Bgmtv.SearchFilter
            { subjectType = Just [Bgmtv.Anime],
              metaTags = Nothing,
              airDate = Nothing
            }
    }

-- | Convert a Bgmtv Subject to Bangumi.
bgmtvSubjectToBangumi :: Subject -> Maybe Bangumi
bgmtvSubjectToBangumi s = mkBgmtvBangumi s.date s.name s.nameCn s.platform s.id s.images.large

-- | Convert a Bgmtv SubjectDetail to Bangumi.
bgmtvDetailToBangumi :: SubjectDetail -> Maybe Bangumi
bgmtvDetailToBangumi d = mkBgmtvBangumi d.date d.name d.nameCn d.platform d.id d.images.large

-- | Map Bgmtv Platform to BangumiKind.
platformToKind :: Bgmtv.Platform -> BangumiKind
platformToKind Bgmtv.TV = Tv
platformToKind Bgmtv.Theatrical = Movie
platformToKind Bgmtv.OVA = Ova
platformToKind (Bgmtv.OtherPlatform _) = Web

mkBgmtvBangumi :: Maybe Day -> Text -> Text -> Bgmtv.Platform -> SubjectId -> Text -> Maybe Bangumi
mkBgmtvBangumi maybeDate name nameCn platform subjectId imageUrl = do
  date <- maybeDate
  let parsed = parseOriginalTitle (name, nameCn)
  pure Bangumi
    { titleChs = parsed.titleChs,
      titleJap = Just parsed.titleJap,
      airDate = date,
      firstAirYear = Nothing,
      season = parsed.season,
      kind = platformToKind platform,
      mikanId = Nothing,
      tmdbId = Nothing,
      bgmtvId = Just (BgmtvId (fromIntegral subjectId.unSubjectId)),
      posterUrl = Just imageUrl,
      totalEpisodes = Nothing
    }
