module Moe.Web.API.DTO.Calendar
  ( CalendarEntry (..),
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Moe.Prelude
import Moe.Web.API.DTO.Bangumi (BangumiResponse)

data CalendarEntry = CalendarEntry
  { weekday :: Int,
    bangumis :: [BangumiResponse]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)
