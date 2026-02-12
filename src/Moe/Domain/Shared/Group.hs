-- | Subtitle group types shared across domains.
module Moe.Domain.Shared.Group
  ( GroupName (..),
    Group (..),
    normalizeGroupName,
    splitGroupNames,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful.Sqlite (FromField (..), ToField (..))
import Moe.Prelude

-- | Subtitle group name
newtype GroupName = GroupName Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, FromJSON, ToJSON, ToSchema, ToText, FromField, ToField)

data Group = Group
  { name :: GroupName,
    aliases :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToText Group where
  toText = toText . name

-- | Normalize a group name to its canonical form.
--
-- >>> normalizeGroupName [Group (GroupName "Lilith-Raws") ["Lilith"]] "lilith"
-- GroupName "Lilith-Raws"
normalizeGroupName :: [Group] -> Text -> GroupName
normalizeGroupName groups raw =
  case find (matchesGroup raw) groups of
    Just g -> g.name
    Nothing -> GroupName raw
  where
    matchesGroup t g =
      T.toLower t == T.toLower (toText g.name)
        || any (\a -> T.toLower t == T.toLower a) g.aliases

-- | Split a group string by @&@ and normalize each part.
--
-- >>> splitGroupNames [Group (GroupName "Lilith-Raws") ["Lilith"]] "Lilith & LoliHouse"
-- [GroupName "Lilith-Raws", GroupName "LoliHouse"]
splitGroupNames :: [Group] -> Text -> [GroupName]
splitGroupNames groups raw =
  map (normalizeGroupName groups . T.strip) $ T.splitOn "&" raw

-- | Serialize a list of GroupName as comma-separated text.
instance ToField [GroupName] where
  toField [] = toField (Nothing @Text)
  toField gs = toField $ T.intercalate "," (map toText gs)

-- | Deserialize comma-separated text into a list of GroupName.
instance FromField [GroupName] where
  fromField f = do
    mText <- fromField @(Maybe Text) f
    pure $ case mText of
      Nothing -> []
      Just t
        | T.null t -> []
        | otherwise -> map (GroupName . T.strip) $ T.splitOn "," t
