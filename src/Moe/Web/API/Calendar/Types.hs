module Moe.Web.API.Calendar.Types
  ( CalendarEntry (..),
    CalendarResponse (..),
  )
where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Moe.Web.API.Bangumi.Types (BangumiResponse)

data CalendarEntry = CalendarEntry
  { weekday :: Int,
    bangumis :: [BangumiResponse]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data CalendarResponse = CalendarResponse
  { season :: Text,
    entries :: [CalendarEntry]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)
