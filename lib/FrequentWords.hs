module FrequentWords
  ( frequentWords
  ) where

import Data.List
import Utils ((^>>>), getMaxFromMap, splitKMers, toFreqTable)

-- | frequentWords : Find all most frequent k-mers in a text.
frequentWords :: String -> Int -> [String]
frequentWords text k =
  splitKMers text k ^>>> toFreqTable ^>>> getMaxFromMap ^>>> reverse
