module PatternCount
  ( pattCount
  ) where

import qualified Data.ByteString.Lazy.Char8 as C

type Text = C.ByteString

type Pattern = C.ByteString

-- | patternCount : given a text and a pattern, we count the number
--   of occurences of that pattern in the text.
pattCount :: Text -> Pattern -> Integer
pattCount text ptrn
  | text /= C.empty =
    if C.take len text == ptrn
      then pattCount (C.drop 1 text) ptrn + 1
      else pattCount (C.drop 1 text) ptrn
  | otherwise = 0
  where
    len = C.length ptrn
