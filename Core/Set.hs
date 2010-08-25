-- |
-- Module    : Core.Set
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- This module implements set of symbols.
module Core.Set
       (
         Set
       , Range(..)
       , fromRanges
       , toRanges
       , empty
       , alphabet
       , member
       , complement
       , union
       , intersect
       , toPartition
       ) where

import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as B
import Core.Partition

-- | Represents set of symbols.
newtype Set s = Set (Pa s)
              deriving (Eq, Ord)

-- | @Range a b@ contains every symbol @s@ which meets @a <= s <= b@.
--
--   Valid range has @a <= b@.
data Range s = Range !s !s
             deriving (Eq, Show)

-- | @'fromRanges' rs@ constructs symbol set with symbols from ranges
--   in the list @rs@.
fromRanges :: Symbol s => [Range s] -> Set s
fromRanges = foldl union empty . map (Set . fromList . rangeToList)
  where
    rangeToList (Range x y)
      | x == minBound = if y == maxBound then [(-1, maxBound)]
                                         else [(-1, y), (0, maxBound)]
      | y == maxBound = [(0, pred x), (-1, maxBound)]
      | otherwise     = [(0, pred x), (-1, y), (0, maxBound)]

-- | Returns ranges with characters inside symbol set.
toRanges :: Symbol s => Set s -> [Range s]
toRanges (Set a) = map intervalToRange $ filterIntervals $ toIntervals a
  where
    filterIntervals             = filter (\(b, _, _) -> b /= 0)
    intervalToRange (_, lo, hi) = Range lo hi

-- | Empty symbol set.
empty :: Symbol s => Set s
empty = Set $ fromList [(0, maxBound)]

-- | Symbol set with all symbols.
alphabet :: Symbol s => Set s
alphabet = Set $ fromList [(-1, maxBound)]

-- | Function @'member' s set@ returns whether symbol @s@ is in @set@.
member :: Symbol s => s -> Set s -> Bool
member s (Set a) = getBlock s a /= 0

-- | Returns complement of the given set.
complement :: Set s -> Set s
complement (Set a) = Set $ pmap B.complement a

-- | Union of symbol sets.
union :: Symbol s => Set s -> Set s -> Set s
union (Set a) (Set b) = Set $ mergeWith (.|.) a b

-- | Intersection of symbol sets.
intersect :: Symbol s => Set s -> Set s -> Set s
intersect (Set a) (Set b) = Set $ mergeWith (.&.) a b

-- | Converts symbol set to partition.
toPartition :: Set s -> Pa s
toPartition (Set a) = a
