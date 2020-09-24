{-# LANGUAGE LambdaCase #-}

module ReverseComplement (reverseComplement) where

-- | complement : Given a nucleotide, output it's complement.
complement :: Char -> Char
complement = \case { 'A' -> 'T';
                     'T' -> 'A';
                     'G' -> 'C';
                     'C' -> 'G'
                   }
-- | reverseComplement : The reverse complement of a string Pattern = p1 ... pn
--                       is the string Pattern = pn* ... p1* formed by taking the
--                       complement of each nucleotide in Pattern, then reversing
--                       the resulting string.
reverseComplement :: String -> String
reverseComplement [] = []
reverseComplement (x:xs) = reverseComplement xs ++ [complement x]
