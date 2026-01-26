module Bangumi.Parser.Types
  ( SubscriptionTitleResult (..),
  )
where

import Bangumi.Internal.Subtitle (SubtitleLang)

data SubscriptionTitleResult = SubscriptionTitleResult
  { episode :: Int
  , subtitleGroup :: Maybe Text
  , subtitles :: [SubtitleLang]
  }
  deriving stock (Show, Eq)
