module Bangumi.Parser.Internal.Bracket
  ( extractBracket,
  )
where

import Data.Text qualified as T

extractBracket :: Text -> Maybe Text
extractBracket input =
  case T.breakOn "[" input of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          case T.breakOn "]" (T.drop 1 rest) of
            (content, _)
              | T.null content -> Nothing
              | otherwise -> Just (T.strip content)
