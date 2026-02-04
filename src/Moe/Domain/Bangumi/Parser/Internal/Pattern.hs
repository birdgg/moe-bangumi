module Moe.Domain.Bangumi.Parser.Internal.Pattern
  ( Pattern (..),
    mkPattern,
    episodePattern,
    titlePattern,
    resolution1080Pattern,
    resolution720Pattern,
    resolution2160Pattern,
    subChsPattern,
    subChtPattern,
    subJpnPattern,
    subEngPattern,
    prefixPattern,
    seasonPattern,
    bracketPattern,
    chsPattern,
    jpPattern,
    enPattern,
    technicalSpecsPattern,
    punctuationPattern,
    multipleSpacesPattern,
    groupPattern,
    matchPattern,
    findPattern,
    findPatternWithPosition,
    extractNumber,
    chineseToNumber,
  )
where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Moe.Prelude
import Regex.Rure (RureMatch (..), hsFind, hsIsMatch)
import Regex.Rure.FFI (rureDefaultFlags)

newtype Pattern = Pattern {unPattern :: ByteString}
  deriving stock (Eq, Show)

mkPattern :: ByteString -> Pattern
mkPattern = Pattern

episodePattern :: Pattern
episodePattern = mkPattern "\\d+"

titlePattern :: Pattern
titlePattern =
  mkPattern $
    BS.concat
      [ "(\\[[^\\]]+\\])?"
      , "(.*|\\[.*])"
      , "(\\(\\d{1,3}\\)|\\[\\d+]|\\[\\d+.?[vV]\\d]|"
      , TE.encodeUtf8 "第\\d+[话話集]|\\[第?\\d+[话話集]]|\\[\\d+.?(END|完)]"
      , "|[Ee][Pp]?\\d+|\\[?"
      , TE.encodeUtf8 "特[別别]篇"
      , "\\]?|\\[?"
      , TE.encodeUtf8 "[總总]集篇"
      , "\\]?|\\d{1,4}-\\d{1,4}|"
      , TE.encodeUtf8 "合集"
      , "| -? ?\\d+([Ee][Nn][Dd]|[vV]\\d|"
      , TE.encodeUtf8 "完"
      , ")?[ \\]\\[-])"
      , "(.*)"
      ]

resolution1080Pattern :: Pattern
resolution1080Pattern = mkPattern "1080|1920x1080"

resolution720Pattern :: Pattern
resolution720Pattern = mkPattern "720|1280x720"

resolution2160Pattern :: Pattern
resolution2160Pattern = mkPattern "2160|4096x2160|4K|4k"

subChsPattern :: Pattern
subChsPattern = mkPattern $ TE.encodeUtf8 "[简中]" <> "|CHS|SC|GB|GBK|GB2312"

subChtPattern :: Pattern
subChtPattern = mkPattern $ TE.encodeUtf8 "繁" <> "|CHT|BIG5"

subJpnPattern :: Pattern
subJpnPattern = mkPattern $ TE.encodeUtf8 "[日]" <> "|JP|JPSC"

subEngPattern :: Pattern
subEngPattern = mkPattern $ "ENG|" <> TE.encodeUtf8 "英语|英文"

prefixPattern :: Pattern
prefixPattern = mkPattern "[^\\w\\s\x4e00-\x9fff\x3040-\x309f\x30a0-\x30ff-]"

seasonPattern :: Pattern
seasonPattern = mkPattern $ "S\\d{1,2}|Season \\d{1,2}|" <> TE.encodeUtf8 "第[一二三四五六七八九十\\d]+[季期]"

bracketPattern :: Pattern
bracketPattern = mkPattern "[\\[\\]]"

chsPattern :: Pattern
chsPattern = mkPattern "[\x4e00-\x9fff]{2,}"

jpPattern :: Pattern
jpPattern = mkPattern "[\x3040-\x309f\x30a0-\x30ff]{2,}"

enPattern :: Pattern
enPattern = mkPattern "[a-zA-Z]{3,}"

technicalSpecsPattern :: Pattern
technicalSpecsPattern = mkPattern "\\d+(-)?[fkb](ps|it|Hz)"

punctuationPattern :: Pattern
punctuationPattern = mkPattern "[^\\w\x4e00-\x9fff\x3040-\x309f\x30a0-\x30ff]"

multipleSpacesPattern :: Pattern
multipleSpacesPattern = mkPattern "\\s{2,}"

groupPattern :: Pattern
groupPattern = mkPattern "^\\[([^\\]]+)\\]"

matchPattern :: Pattern -> Text -> Bool
matchPattern pat input =
  case hsIsMatch rureDefaultFlags (unPattern pat) (TE.encodeUtf8 input) of
    Right b -> b
    Left _ -> False

findPattern :: Pattern -> Text -> Maybe Text
findPattern pat input =
  case hsFind rureDefaultFlags (unPattern pat) (TE.encodeUtf8 input) of
    Right (Just m) ->
      let bs = TE.encodeUtf8 input
          matched = BS.take (fromIntegral (end m - start m)) (BS.drop (fromIntegral (start m)) bs)
       in Just (TE.decodeUtf8 matched)
    _ -> Nothing

findPatternWithPosition :: Pattern -> Text -> Maybe (Text, Text)
findPatternWithPosition pat input =
  case hsFind rureDefaultFlags (unPattern pat) (TE.encodeUtf8 input) of
    Right (Just m) ->
      let bs = TE.encodeUtf8 input
          before = TE.decodeUtf8 $ BS.take (fromIntegral (start m)) bs
          matched = TE.decodeUtf8 $ BS.take (fromIntegral (end m - start m)) (BS.drop (fromIntegral (start m)) bs)
       in Just (before, matched)
    _ -> Nothing

extractNumber :: Text -> Maybe Word32
extractNumber t =
  let digits = T.filter (`elem` ['0' .. '9']) t
   in if T.null digits then Nothing else readMaybe (T.unpack digits)

chineseToNumber :: Text -> Maybe Word32
chineseToNumber t =
  let chars = T.unpack t
      digits = filter (`elem` chineseDigits) chars
   in case digits of
        [c] -> lookup c chineseDigitMap
        [c1, c2]
          | c1 == '十' -> (+ 10) <$> lookup c2 chineseDigitMap
          | c2 == '十' -> (* 10) <$> lookup c1 chineseDigitMap
          | otherwise -> Nothing
        [c1, _, c3]
          | lookup c1 chineseDigitMap == Just 1 -> Nothing
          | otherwise -> do
              tens <- lookup c1 chineseDigitMap
              ones <- lookup c3 chineseDigitMap
              Just (tens * 10 + ones)
        _ -> Nothing
  where
    chineseDigits :: String
    chineseDigits = "一二三四五六七八九十"
    chineseDigitMap :: [(Char, Word32)]
    chineseDigitMap =
      [ ('一', 1),
        ('二', 2),
        ('三', 3),
        ('四', 4),
        ('五', 5),
        ('六', 6),
        ('七', 7),
        ('八', 8),
        ('九', 9),
        ('十', 10)
      ]
