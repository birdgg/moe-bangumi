module Moe.Bangumi.Internal.Extension
  ( VideoExt (..),
    SubtitleExt (..),
    FileType (..),
  )
where

import Moe.Bangumi.Internal.Subtitle (SubtitleLang)

data VideoExt
  = MKV
  | MP4
  | AVI
  | WEBM
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance ToText VideoExt where
  toText MKV = "mkv"
  toText MP4 = "mp4"
  toText AVI = "avi"
  toText WEBM = "webm"

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

data FileType
  = Video VideoExt
  | Subtitle SubtitleLang SubtitleExt
  deriving stock (Eq, Show)
