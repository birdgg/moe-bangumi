module Moe.Bangumi.Internal.Subtitle
  ( SubtitleLang (..),
    SubtitleList,
  )
where

import Data.Text.Display

data SubtitleLang
  = CHS
  | CHT
  | JPN
  | ENG
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Display SubtitleLang where
  displayBuilder CHS = "简"
  displayBuilder CHT = "繁"
  displayBuilder JPN = "日"
  displayBuilder ENG = "英"

instance ToText SubtitleLang where
  toText CHS = "chs"
  toText CHT = "cht"
  toText JPN = "jpn"
  toText ENG = "eng"

type SubtitleList = [SubtitleLang]
