-- | Subtitle language types shared across domains.
module Moe.Domain.Shared.Subtitle
  ( Subtitle (..),
    SubtitleList,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Effectful.Sqlite (FromField (..), ToField (..))
import Moe.Prelude

data Subtitle
  = CHS
  | CHT
  | JPN
  | ENG
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToSchema)

instance Display Subtitle where
  displayBuilder CHS = "简"
  displayBuilder CHT = "繁"
  displayBuilder JPN = "日"
  displayBuilder ENG = "英"

instance ToText Subtitle where
  toText CHS = "chs"
  toText CHT = "cht"
  toText JPN = "jpn"
  toText ENG = "eng"

instance FromText Subtitle where
  fromText = inverseMap toText . T.toLower . T.strip

instance FromJSON Subtitle where
  parseJSON = withText "Subtitle" $ \t ->
    case fromText t of
      Just lang -> pure lang
      Nothing -> fail $ "Invalid Subtitle: " <> toString t

instance ToJSON Subtitle where
  toJSON = toJSON . toText

-- | Serialize a list of Subtitle as comma-separated text.
instance ToField [Subtitle] where
  toField [] = toField (Nothing @Text)
  toField ls = toField $ T.intercalate "," (map toText ls)

-- | Deserialize comma-separated text into a list of Subtitle.
instance FromField [Subtitle] where
  fromField f = do
    mText <- fromField @(Maybe Text) f
    pure $ case mText of
      Nothing -> []
      Just t
        | T.null t -> []
        | otherwise -> mapMaybe (fromText . T.strip) $ T.splitOn "," t

type SubtitleList = [Subtitle]
