-- | Detect the script/language of bangumi titles.
module Moe.Domain.Bangumi.Language
  ( Script (..),
    detectScript,
    isCJK,
    isHiragana,
    isKatakana,
    isJapanese,
    isLatin,
  )
where

import Moe.Prelude

-- | Script categories for bangumi title classification.
data Script
  = -- | Contains hiragana or katakana (Japanese)
    Japanese
  | -- | CJK characters only, no kana (likely Chinese)
    Chinese
  | -- | Latin alphabet (English or Romaji)
    Latin
  | -- | Mixed or unrecognized scripts
    Mixed
  deriving stock (Eq, Show, Ord)

-- | Detect the dominant script of a text.
detectScript :: Text -> Script
detectScript t =
  let chars = filter (not . isIgnored) $ toString t
   in case chars of
        [] -> Mixed
        _ ->
          let hasKana = any (\c -> isHiragana c || isKatakana c) chars
              hasCJK = any isCJK chars
              hasLat = any isLatin chars
           in classify hasKana hasCJK hasLat
  where
    classify hasKana hasCJK hasLat
      | hasKana = Japanese
      | hasCJK && not hasLat = Chinese
      | hasLat && not hasCJK = Latin
      | otherwise = Mixed

    isIgnored c =
      let n = ord c
       in n <= 0x7E && not (isLatin c) -- ASCII punctuation, digits, spaces

-- | CJK Unified Ideographs and Extension A.
isCJK :: Char -> Bool
isCJK c =
  let n = ord c
   in (n >= 0x4E00 && n <= 0x9FFF)
        || (n >= 0x3400 && n <= 0x4DBF)

-- | Hiragana block.
isHiragana :: Char -> Bool
isHiragana c = let n = ord c in n >= 0x3040 && n <= 0x309F

-- | Katakana block.
isKatakana :: Char -> Bool
isKatakana c = let n = ord c in n >= 0x30A0 && n <= 0x30FF

-- | Japanese text: contains kana or CJK with kana context.
isJapanese :: Char -> Bool
isJapanese c = isHiragana c || isKatakana c

-- | Basic Latin and Latin Extended-A/B.
isLatin :: Char -> Bool
isLatin c =
  let n = ord c
   in (n >= 0x41 && n <= 0x5A) -- A-Z
        || (n >= 0x61 && n <= 0x7A) -- a-z
        || (n >= 0xC0 && n <= 0x024F) -- Latin Extended
