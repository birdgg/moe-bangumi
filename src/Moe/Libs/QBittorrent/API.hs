{-# LANGUAGE FieldSelectors #-}

module Moe.Libs.QBittorrent.API
  ( QBittorrentRoutes (..),
    AuthRoutes (..),
    AppRoutes (..),
    TorrentsRoutes (..),
    mkBaseUrl,
  )
where

import Data.Text qualified as T
import Moe.Libs.QBittorrent.Types
import Moe.Prelude hiding ((:>))
import Servant.API
  ( FormUrlEncoded,
    Get,
    JSON,
    NamedRoutes,
    NoContent,
    PlainText,
    Post,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    Strict,
    type (:-),
    type (:>),
  )
import Servant.Client (BaseUrl (..), Scheme (..))

data QBittorrentRoutes mode = QBittorrentRoutes
  { auth :: mode :- "auth" :> NamedRoutes AuthRoutes,
    torrents :: mode :- "torrents" :> NamedRoutes TorrentsRoutes,
    app :: mode :- "app" :> NamedRoutes AppRoutes
  }
  deriving stock (Generic)

data AuthRoutes mode = AuthRoutes
  { login ::
      mode
        :- "login"
          :> ReqBody '[FormUrlEncoded] LoginForm
          :> Post '[PlainText] Text
  }
  deriving stock (Generic)

data AppRoutes mode = AppRoutes
  { version ::
      mode
        :- "version"
          :> Get '[PlainText] Text
  }
  deriving stock (Generic)

data TorrentsRoutes mode = TorrentsRoutes
  { add ::
      mode
        :- "add"
          :> ReqBody '[FormUrlEncoded] AddTorrentRequest
          :> Post '[PlainText] Text,
    info ::
      mode
        :- "info"
          :> QueryParam "filter" Text
          :> QueryParam "category" Text
          :> QueryParam "tag" Text
          :> QueryParam "hashes" Text
          :> Get '[JSON] [TorrentInfo],
    files ::
      mode
        :- "files"
          :> QueryParam' '[Required, Strict] "hash" InfoHash
          :> Get '[JSON] [TorrentContent],
    stop ::
      mode
        :- "stop"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent,
    start ::
      mode
        :- "start"
          :> ReqBody '[FormUrlEncoded] HashesForm
          :> Post '[PlainText] NoContent,
    delete ::
      mode
        :- "delete"
          :> ReqBody '[FormUrlEncoded] DeleteTorrentsForm
          :> Post '[PlainText] NoContent,
    addTags ::
      mode
        :- "addTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent,
    removeTags ::
      mode
        :- "removeTags"
          :> ReqBody '[FormUrlEncoded] TagsForm
          :> Post '[PlainText] NoContent,
    renameFile ::
      mode
        :- "renameFile"
          :> ReqBody '[FormUrlEncoded] RenameFileForm
          :> Post '[PlainText] NoContent,
    renameFolder ::
      mode
        :- "renameFolder"
          :> ReqBody '[FormUrlEncoded] RenameFolderForm
          :> Post '[PlainText] NoContent,
    setLocation ::
      mode
        :- "setLocation"
          :> ReqBody '[FormUrlEncoded] SetLocationForm
          :> Post '[PlainText] NoContent,
    setFilePrio ::
      mode
        :- "filePrio"
          :> ReqBody '[FormUrlEncoded] FilePrioForm
          :> Post '[PlainText] NoContent
  }
  deriving stock (Generic)

mkBaseUrl :: QBConfig -> BaseUrl
mkBaseUrl cfg =
  BaseUrl
    { baseUrlScheme = if cfg.useTLS then Https else Http,
      baseUrlHost = T.unpack cfg.host,
      baseUrlPort = cfg.port,
      baseUrlPath = "/api/v2"
    }
