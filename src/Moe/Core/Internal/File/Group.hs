module Moe.Core.Internal.File.Group
  ( Group (..),
    normalizeGroup,
    knownGroups,
  )
where

import Data.Text qualified as T

data Group = Group
  { name :: Text,
    aliases :: [Text]
  }
  deriving stock (Show, Eq)

knownGroups :: [Group]
knownGroups =
  [ Group "SweetSub" [],
    Group "千夏字幕组" [],
    Group "拨雪寻春" ["❀拨雪寻春❀"],
    Group "喵萌奶茶屋" [],
    Group "LoliHouse" [],
    Group "北宇治" ["北宇治字幕组"],
    Group "诸神字幕组" [],
    Group "霜庭云花" [],
    Group "桜都字幕组" [],
    Group "澄空学园" []
  ]

normalizeGroup :: Text -> Text
normalizeGroup raw =
  case find (matchesGroup raw) knownGroups of
    Just g -> g.name
    Nothing -> raw
  where
    matchesGroup t g =
      T.toLower t == T.toLower g.name
        || any (\a -> T.toLower t == T.toLower a) g.aliases
