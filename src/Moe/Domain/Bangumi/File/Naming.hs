module Moe.Domain.Bangumi.File.Naming
  ( generatePath,
    generateBaseName,
    generateFileName,
    generateFullPath,
    sanitizeName,
  )
where

import Data.Text qualified as T
import Moe.Domain.Bangumi.File.Types
import Moe.Prelude
import System.FilePath ((</>))

generatePath :: BangumiFile -> FilePath
generatePath file = case file.content of
  Episode epType -> showBaseName file.meta </> episodeDir epType
  Extra _ -> showBaseName file.meta </> "extras"
  TrailerItem _ -> showBaseName file.meta </> "trailers"
  Movie movieYear -> T.unpack $ nameWithYear file.meta.name movieYear

-- | Generate file base name without extension.
generateBaseName :: BangumiFile -> FilePath
generateBaseName file = toString $ case file.content of
  Episode epType -> episodeBaseName file.meta.name epType
  Extra extra -> extraBaseName extra
  TrailerItem trailer -> trailerBaseName trailer
  Movie movieYear -> nameWithYear file.meta.name movieYear

generateFileName :: BangumiFile -> FilePath
generateFileName file = T.unpack $ baseName <> extension
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
showBaseName meta = T.unpack $ case meta.year of
  Just year -> nameWithYear meta.name year
  Nothing -> sanitizeName meta.name

nameWithYear :: Text -> Year -> Text
nameWithYear name y = sanitizeName name <> " (" <> toText y <> ")"

episodeDir :: EpisodeType -> FilePath
episodeDir = \case
  Regular (SeasonNumber s) _ -> T.unpack $ "Season " <> padded s
  Special _ -> "Season 00"

episodeBaseName :: Text -> EpisodeType -> Text
episodeBaseName name = \case
  Regular (SeasonNumber s) ep ->
    sanitizeName name <> " - S" <> padded s <> "E" <> toText ep
  Special ep ->
    sanitizeName name <> " - S00E" <> toText ep

extraBaseName :: ExtraContent -> Text
extraBaseName = \case
  NCOP Nothing -> "NCOP"
  NCOP (Just (ExtraIndex i)) -> "NCOP" <> show i
  NCED Nothing -> "NCED"
  NCED (Just (ExtraIndex i)) -> "NCED" <> show i
  Menu Nothing -> "Menu"
  Menu (Just (ExtraIndex i)) -> "Menu" <> show i

trailerBaseName :: TrailerContent -> Text
trailerBaseName = \case
  PV (ExtraIndex i) -> "PV" <> show i
  Preview -> "Preview"
  Trailer -> "Trailer"
  CM Nothing -> "CM"
  CM (Just (ExtraIndex i)) -> "CM" <> show i

fileTypeExtension :: FileType -> Text
fileTypeExtension = \case
  Video ext -> "." <> toText ext
  Subtitle lang ext -> "." <> toText lang <> "." <> toText ext

padded :: Word8 -> Text
padded n
  | n < 10 = "0" <> show n
  | otherwise = show n
