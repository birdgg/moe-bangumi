module Moe.Domain.Bangumi.Parser.File
  ( ParsedInfo (..),
    parseInfo,
    extractSeason,
    extractEpisode,
    extractGroup,
  )
where

import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import Moe.Domain.Bangumi.Parser.Internal.Pattern
import Moe.Domain.Bangumi.Episode (EpisodeNumber (..))
import Regex.Rure (RureMatch (..), hsFind)
import Regex.Rure.FFI (rureDefaultFlags)
import Text.Read (readMaybe)

data ParsedInfo = ParsedInfo
  { title :: Text,
    season :: Maybe Word32,
    episodeNumber :: Maybe EpisodeNumber,
    group :: Maybe Text
  }
  deriving stock (Eq, Show)

parseInfo :: Text -> ParsedInfo
parseInfo input =
  ParsedInfo
    { title = input,
      season = extractSeason input,
      episodeNumber = extractEpisode input,
      group = extractGroup input
    }

extractSeason :: Text -> Maybe Word32
extractSeason input = do
  matched <- findPattern seasonPattern input
  extractNumber matched

extractEpisode :: Text -> Maybe EpisodeNumber
extractEpisode input = do
  matched <- findPattern episodePattern input
  EpisodeNumber <$> readMaybe (T.unpack matched)

extractGroup :: Text -> Maybe Text
extractGroup input =
  case hsFind rureDefaultFlags (unPattern groupPattern) (TE.encodeUtf8 input) of
    Right (Just m) ->
      let bs = TE.encodeUtf8 input
          matched = BS.take (fromIntegral (end m - start m)) (BS.drop (fromIntegral (start m)) bs)
          txt = TE.decodeUtf8 matched
       in if T.null txt then Nothing else Just (stripBrackets txt)
    _ -> Nothing
  where
    stripBrackets t = fromMaybe t (T.stripPrefix "[" t >>= T.stripSuffix "]")
