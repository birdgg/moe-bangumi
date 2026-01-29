module Moe.Domain.File.Naming
  ( generatePath,
    generateFileName,
    generateFullPath,
    sanitizeName,
  )
where

import Data.Text qualified as T
import Moe.Domain.File.Types
import System.FilePath ((</>))

generatePath :: BangumiFile -> FilePath
generatePath file = case file.content of
  Episode epType -> showBaseName file.meta </> episodeDir epType
  Extra _ -> showBaseName file.meta </> "extras"
  TrailerItem _ -> showBaseName file.meta </> "trailers"
  Movie movieYear -> toString $ nameWithYear file.meta.name movieYear

generateFileName :: BangumiFile -> FilePath
generateFileName file = toString $ baseName <> extension
  where
    baseName = case file.content of
      Episode epType -> episodeBaseName file.meta.name epType
      Extra extra -> extraBaseName extra
      TrailerItem trailer -> trailerBaseName trailer
      Movie movieYear -> nameWithYear file.meta.name movieYear
    extension = fileTypeExtension file.fileType

generateFullPath :: BangumiFile -> FilePath
generateFullPath file = generatePath file </> generateFileName file

sanitizeName :: Text -> Text
sanitizeName = T.map replaceChar . T.filter (`notElem` forbiddenChars)
  where
    forbiddenChars :: [Char]
    forbiddenChars = "<>|?*\""
    replaceChar ':' = '-'
    replaceChar '/' = '-'
    replaceChar '\\' = '-'
    replaceChar c = c

showBaseName :: BangumiMeta -> FilePath
showBaseName meta = toString $ case meta.year of
  Just year -> nameWithYear meta.name year
  Nothing -> sanitizeName meta.name

nameWithYear :: Text -> Year -> Text
nameWithYear name (Year y) = sanitizeName name <> " (" <> show y <> ")"

episodeDir :: EpisodeType -> FilePath
episodeDir = \case
  Regular (SeasonNum s) _ -> toString $ "Season " <> padded s
  Special _ -> "Season 00"

episodeBaseName :: Text -> EpisodeType -> Text
episodeBaseName name = \case
  Regular (SeasonNum s) (EpisodeNum e) ->
    sanitizeName name <> " - S" <> padded s <> "E" <> padded e
  Special (EpisodeNum e) ->
    sanitizeName name <> " - S00E" <> padded e

extraBaseName :: ExtraContent -> Text
extraBaseName = \case
  NCOP Nothing -> "NCOP"
  NCOP (Just (Index i)) -> "NCOP" <> show i
  NCED Nothing -> "NCED"
  NCED (Just (Index i)) -> "NCED" <> show i
  Menu Nothing -> "Menu"
  Menu (Just (Index i)) -> "Menu" <> show i

trailerBaseName :: TrailerContent -> Text
trailerBaseName = \case
  PV (Index i) -> "PV" <> show i
  Preview -> "Preview"
  Trailer -> "Trailer"
  CM Nothing -> "CM"
  CM (Just (Index i)) -> "CM" <> show i

fileTypeExtension :: FileType -> Text
fileTypeExtension = \case
  Video ext -> "." <> toText ext
  Subtitle lang ext -> "." <> toText lang <> "." <> toText ext

padded :: Word8 -> Text
padded n
  | n < 10 = "0" <> show n
  | otherwise = show n
