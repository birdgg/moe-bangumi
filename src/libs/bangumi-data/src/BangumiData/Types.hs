module BangumiData.Types
  ( BangumiDataItem (..),
    BangumiDataSite (..),
    TitleTranslate (..),
  )
where

import Data.Aeson

data BangumiDataItem = BangumiDataItem
  { title :: Text,
    titleTranslate :: TitleTranslate,
    itemType :: Text,
    lang :: Text,
    begin :: Maybe Text,
    sites :: [BangumiDataSite]
  }
  deriving stock (Eq, Show)

instance FromJSON BangumiDataItem where
  parseJSON = withObject "BangumiDataItem" $ \v ->
    BangumiDataItem
      <$> v .: "title"
      <*> v .:? "titleTranslate" .!= emptyTitleTranslate
      <*> v .: "type"
      <*> v .: "lang"
      <*> v .:? "begin"
      <*> v .:? "sites" .!= []

data BangumiDataSite = BangumiDataSite
  { site :: Text,
    siteId :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON BangumiDataSite where
  parseJSON = withObject "BangumiDataSite" $ \v ->
    BangumiDataSite
      <$> v .: "site"
      <*> v .: "id"

data TitleTranslate = TitleTranslate
  { zhHans :: [Text],
    zhHant :: [Text]
  }
  deriving stock (Eq, Show)

emptyTitleTranslate :: TitleTranslate
emptyTitleTranslate = TitleTranslate [] []

instance FromJSON TitleTranslate where
  parseJSON = withObject "TitleTranslate" $ \v ->
    TitleTranslate
      <$> v .:? "zh-Hans" .!= []
      <*> v .:? "zh-Hant" .!= []
