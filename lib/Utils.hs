module Utils where

import qualified Data.Map.Strict as M

type FreqTable a = M.Map a Int

infixl 0 ^>>>
(^>>>) :: a -> (a -> c) -> c
(^>>>) = flip ($)

-- | splitKMers : Takes a string and splits it into k-mers.
splitKMers :: [a] -> Int -> [[a]]
splitKMers [] n = []
splitKMers (x:xs) n | len < n = []
                    | otherwise = as : splitKMers xs n
                    where (as, bs) = splitAt n (x:xs)
                          len = length (x:xs)

-- | toFreqTable : Input a string and generate a frequency table.
--                 We make use of the Map datastructure.
toFreqTable :: Ord a => [a] -> FreqTable a
toFreqTable = foldr f M.empty
            where f x m = M.insertWith (+) x 1 m

-- | getMaxFromMap : Given a map, find all Keys that have the
--                   largest value.
getMaxFromMap m = go [] Nothing (M.toList m)
  where
    go ks _        []           = ks
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest
