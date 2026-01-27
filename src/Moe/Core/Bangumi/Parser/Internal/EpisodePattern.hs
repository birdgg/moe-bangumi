module Moe.Core.Bangumi.Parser.Internal.EpisodePattern
  ( episodePatterns,
    extractFirstDigits,
    extractAfterE,
    extractAfterEP,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T

episodePatterns :: [(Text, Text -> Maybe Int)]
episodePatterns =
  [ ("S[0-9]+E([0-9]+)", extractAfterE)
  , ("\\[([0-9]+)\\.?(END|完)\\]", extractFirstDigits)
  , ("\\[第?([0-9]+)[集话話]?\\]", extractFirstDigits)
  , ("\\(([0-9]+)\\)", extractFirstDigits)
  , ("[Ee][Pp]?([0-9]+)", extractAfterEP)
  , ("第([0-9]+)[话話集]", extractFirstDigits)
  , (" - ([0-9]+)", extractFirstDigits)
  , (" ([0-9]{1,3})($|[^0-9pPkK])", extractFirstDigits)
  ]

extractFirstDigits :: Text -> Maybe Int
extractFirstDigits t =
  let digits = T.takeWhile isDigit $ T.dropWhile (not . isDigit) t
   in if T.null digits then Nothing else readMaybe (toString digits)

extractAfterE :: Text -> Maybe Int
extractAfterE t =
  case T.breakOn "E" (T.toUpper t) of
    (_, rest) | T.length rest > 1 ->
      let digits = T.takeWhile isDigit (T.drop 1 rest)
       in if T.null digits then Nothing else readMaybe (toString digits)
    _ -> Nothing

extractAfterEP :: Text -> Maybe Int
extractAfterEP t =
  let upper = T.toUpper t
      afterE = T.drop 1 $ snd $ T.breakOn "E" upper
      afterP = if T.isPrefixOf "P" afterE then T.drop 1 afterE else afterE
      digits = T.takeWhile isDigit afterP
   in if T.null digits then Nothing else readMaybe (toString digits)
