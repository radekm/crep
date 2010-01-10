-- |
-- Module    : Core.Partition
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- Partitioning of the alphabet.
--
module Core.Partition
  ( Partition
  , fromCharSet
  , toCharSets
  , maxBlock
  , intersectTwoPs
  , intersectManyPs
  ) where

import Core.SymbSet

-- |Represents partition of the alphabet.
newtype Partition = P [CharSet]

-- |Converts character set to partition with two blocks.
fromCharSet :: CharSet -> Partition
fromCharSet cs = P $ filter (/= empty) [cs, complement cs]

-- |Partition with one block.
maxBlock :: Partition
maxBlock = P [alphabet]
{-# INLINE maxBlock #-}

-- |Computes intersection of two partitions.
intersectTwoPs :: Partition -> Partition -> Partition
intersectTwoPs (P as) (P bs) = P $ remEmpty [intersect a b | a <- as, b <- bs]
  where
    remEmpty = filter (/= empty)

-- |Computes intersection of all partitions in the given list.
intersectManyPs :: [Partition] -> Partition
intersectManyPs = foldl intersectTwoPs maxBlock

-- |Converts partition to list of character sets (one per block of partition).
toCharSets :: Partition -> [CharSet]
toCharSets (P css) = css
{-# INLINE toCharSets #-}

