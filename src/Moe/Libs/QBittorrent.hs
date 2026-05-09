module Moe.Libs.QBittorrent
  ( login,
    addTorrent,
    getTorrents,
    getTorrentContents,
    stopTorrents,
    startTorrents,
    deleteTorrents,
    addTags,
    removeTags,
    renameFile,
    renameFolder,
    setLocation,
    setFilePriority,
    getVersion,
    initQBClientWith,
    runQB,
    module Moe.Libs.QBittorrent.Types,
    ClientM,
    ClientError,
  )
where

import Data.Text qualified as T
import Moe.Libs.QBittorrent.API qualified as API
import Moe.Libs.QBittorrent.Types
import Moe.Prelude hiding (renameFile)
import Network.HTTP.Client (Manager)
import Servant.API (NoContent)
import Servant.Client (ClientError, ClientM, runClientM)
import Servant.Client qualified as Servant
import Servant.Client.Generic (AsClientT, genericClient)

initQBClientWith :: Manager -> QBConfig -> IO QBClient
initQBClientWith manager cfg = do
  jar <- newTVarIO mempty
  let baseEnv = Servant.mkClientEnv manager (API.mkBaseUrl cfg)
      env = baseEnv {Servant.cookieJar = Just jar}
  pure QBClient {clientEnv = env, cookieJar = jar, config = cfg}

runQB :: QBClient -> ClientM a -> IO (Either ClientError a)
runQB client action = runClientM action client.clientEnv

qbClient :: API.QBittorrentRoutes (AsClientT ClientM)
qbClient = genericClient

login :: QBConfig -> ClientM Text
login cfg = qbClient.auth.login (LoginForm cfg.credential.username cfg.credential.password)

addTorrent :: AddTorrentRequest -> ClientM Text
addTorrent = qbClient.torrents.add

getTorrents :: Maybe TorrentInfoRequest -> ClientM [TorrentInfo]
getTorrents mReq = case mReq of
  Nothing -> qbClient.torrents.info Nothing Nothing Nothing Nothing
  Just req ->
    qbClient.torrents.info
      (torrentFilterToText <$> req.filter_)
      req.category
      (fmap (.unTag) req.tag)
      (fmap hashesToText req.hashes)

getTorrentContents :: InfoHash -> ClientM [TorrentContent]
getTorrentContents = qbClient.torrents.files

stopTorrents :: [InfoHash] -> ClientM NoContent
stopTorrents hashes = qbClient.torrents.stop (HashesForm hashes)

startTorrents :: [InfoHash] -> ClientM NoContent
startTorrents hashes = qbClient.torrents.start (HashesForm hashes)

deleteTorrents :: [InfoHash] -> Bool -> ClientM NoContent
deleteTorrents hashes deleteFiles =
  qbClient.torrents.delete (DeleteTorrentsForm hashes deleteFiles)

addTags :: [InfoHash] -> [Tag] -> ClientM NoContent
addTags hashes tags = qbClient.torrents.addTags (TagsForm hashes tags)

removeTags :: [InfoHash] -> [Tag] -> ClientM NoContent
removeTags hashes tags = qbClient.torrents.removeTags (TagsForm hashes tags)

renameFile :: InfoHash -> Text -> Text -> ClientM NoContent
renameFile hash oldPath newPath =
  qbClient.torrents.renameFile (RenameFileForm hash oldPath newPath)

renameFolder :: InfoHash -> Text -> Text -> ClientM NoContent
renameFolder hash oldPath newPath =
  qbClient.torrents.renameFolder (RenameFolderForm hash oldPath newPath)

setLocation :: [InfoHash] -> Text -> ClientM NoContent
setLocation hashes location =
  qbClient.torrents.setLocation (SetLocationForm hashes location)

setFilePriority :: InfoHash -> [Int] -> Int -> ClientM NoContent
setFilePriority hash fileIds priority =
  qbClient.torrents.setFilePrio (FilePrioForm hash (T.intercalate "|" $ map (show @Text) fileIds) priority)

getVersion :: ClientM Text
getVersion = qbClient.app.version
