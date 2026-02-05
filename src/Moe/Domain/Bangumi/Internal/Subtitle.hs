module Moe.Domain.Bangumi.Internal.Subtitle
  ( SubtitleLang (..),
    SubtitleList,
    SubtitleExt (..),
    parseSubtitleLang,
    defaultSubtitlePriority,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Moe.Prelude

data SubtitleLang
  = CHS
  | CHT
  | JPN
  | ENG
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToSchema)

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

instance FromJSON SubtitleLang where
  parseJSON = withText "SubtitleLang" $ \t ->
    case parseSubtitleLang t of
      Just lang -> pure lang
      Nothing -> fail $ "Invalid SubtitleLang: " <> toString t

instance ToJSON SubtitleLang where
  toJSON = toJSON . toText

-- | Parse subtitle language from text
parseSubtitleLang :: Text -> Maybe SubtitleLang
parseSubtitleLang t = case T.toLower (T.strip t) of
  "chs" -> Just CHS
  "cht" -> Just CHT
  "jpn" -> Just JPN
  "eng" -> Just ENG
  _ -> Nothing

type SubtitleList = [SubtitleLang]

defaultSubtitlePriority :: [SubtitleList]
defaultSubtitlePriority = [[CHS, JPN], [CHS], [CHT, JPN], [CHT]]

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
