module Moe.Web.API.Bangumi.Types
  ( BangumiResponse (..),
    toBangumiResponse,
  )
where

import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Text.Conversions (ToText (..))
import Data.Time.Calendar (Day)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Moe.Domain.Bangumi.Types qualified as Types
import Data.Aeson (ToJSON)

data BangumiResponse = BangumiResponse
  { id :: Maybe Int64,
    titleChs :: Text,
    titleJap :: Maybe Text,
    airDate :: Maybe Day,
    seasonNumber :: Maybe Word32,
    kind :: Text,
    posterUrl :: Maybe Text,
    bangumiSeason :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

{- HLINT ignore toBangumiResponse "Redundant id" -}
toBangumiResponse :: Types.Bangumi -> BangumiResponse
toBangumiResponse b =
  BangumiResponse
    { id = fmap coerce b.id,
      titleChs = b.titleChs,
      titleJap = b.titleJap,
      airDate = b.airDate,
      seasonNumber = b.seasonNumber,
      kind = toText b.kind,
      posterUrl = b.posterUrl,
      bangumiSeason = Types.bangumiSeasonToText <$> Types.getBangumiSeason b
    }
