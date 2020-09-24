module BioKell
  ( performPattCount
  , performFrequentWords
  , performReverseComplement
  ) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (intersperse)
import FrequentWords (frequentWords)
import Helpers ((^>>>))
import PatternCount (pattCount)
import ReverseComplement (reverseComplement)
import System.IO (readFile)

-- | performPattCount : Input data from a file and perform Pattern Counting.
--   Input format:
--                 TEXT
--                 PATTERN
performPattCount :: IO ()
performPattCount = do
  filepath <- getLine
  [text, ptrn] <- C.lines <$> C.readFile filepath
  pattCount text ptrn ^>>> print

-- | performFrequentWords : Input data from a file and find the most
--                          frequent k-mers.
--   Input format:
--                 TEXT
--                 PATTERN
performFrequentWords :: IO ()
performFrequentWords = do
  filepath <- getLine
  [text, k] <- lines <$> readFile filepath
  frequentWords text (read k) ^>>> intersperse " " ^>>> unwords ^>>> putStrLn

-- | performReverseComplement : Input data from a file and find the
--                              reverse complement of the DNA.
--   Input format:
--                 TEXT
performReverseComplement :: IO ()
performReverseComplement = do
  filepath <- getLine
  text <- readFile filepath
  reverseComplement text ^>>> putStrLn
