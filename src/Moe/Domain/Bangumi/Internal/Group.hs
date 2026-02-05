module Moe.Domain.Bangumi.Internal.Group
  ( GroupName (..),
    Group (..),
    normalizeGroupName,
    knownGroups,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Moe.Prelude

-- | Subtitle group name
newtype GroupName = GroupName Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, ToJSON, ToSchema)

instance FromJSON GroupName where
  parseJSON = withText "GroupName" (pure . GroupName)

instance ToText GroupName where
  toText (GroupName t) = t

data Group = Group
  { name :: GroupName,
    aliases :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance ToText Group where
  toText = toText . name

knownGroups :: [Group]
knownGroups =
  [ Group (GroupName "SweetSub") [],
    Group (GroupName "千夏字幕组") [],
    Group (GroupName "拨雪寻春") ["❀拨雪寻春❀"],
    Group (GroupName "喵萌奶茶屋") [],
    Group (GroupName "LoliHouse") [],
    Group (GroupName "北宇治") ["北宇治字幕组"],
    Group (GroupName "诸神字幕组") [],
    Group (GroupName "霜庭云花") [],
    Group (GroupName "桜都字幕组") [],
    Group (GroupName "澄空学园") []
  ]

-- | Normalize a group name to its canonical form
normalizeGroupName :: Text -> GroupName
normalizeGroupName raw =
  case find (matchesGroup raw) knownGroups of
    Just g -> g.name
    Nothing -> GroupName raw
  where
    matchesGroup t g =
      T.toLower t == T.toLower (toText g.name)
        || any (\a -> T.toLower t == T.toLower a) g.aliases
