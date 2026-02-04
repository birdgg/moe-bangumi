module Moe.Domain.Bangumi.Internal.Subtitle
  ( SubtitleLang (..),
    SubtitleList,
    SubtitleExt (..),
  )
where

import Data.Text.Display (Display (..))
import Moe.Prelude

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

data SubtitleExt
  = SRT
  | ASS
  | SSA
  | SUB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToText SubtitleExt where
  toText SRT = "srt"
  toText ASS = "ass"
  toText SSA = "ssa"
  toText SUB = "sub"
