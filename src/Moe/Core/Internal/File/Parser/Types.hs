module Moe.Core.Internal.File.Parser.Types
  ( SubscriptionTitleResult (..),
  )
where

import Moe.Core.File.Types (SubtitleLang)

data SubscriptionTitleResult = SubscriptionTitleResult
  { episode :: Int,
    subtitleGroup :: Maybe Text,
    subtitles :: [SubtitleLang]
  }
  deriving stock (Show, Eq)
