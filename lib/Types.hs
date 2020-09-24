{-# LANGUAGE LambdaCase #-}
module Types where

data Nucleotide = A | G | C | T
                deriving (Eq, Show)

type DNA = [Nucleotide]
