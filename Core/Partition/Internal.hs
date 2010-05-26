-- |
-- Module    : Core.Partition.Internal
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- This module contains type @'Pa' e@ for partitioning values of type @e@.
-- Currently only partitioning for type @Char@ is supported.
--
-- Partitions form commutative monoid with intersection, neutral element
-- is the partition with just one block which contains all values.
-- Intersection for two partitions @as@, @bs@ is defined as
-- @[intersectBlocks a b | a <- as, b <-bs]@ where @a@ and @b@ are blocks.
--
-- Example: Lets have two partitions @as == [{a e-f}, {b-d}, {g-z}]@,
-- @bs == [{a-b}, {c-f}, {g-z}]@. Their intersection is
-- @mappend as bs == [{a} {b} {e-f} {c-d} {g-z}]@.
--
-- Another example: @as == [{a-b x-z}, {c-m}, {n-w}]@,
-- @bs == [{a d-f}, {b-c g-z}]@
-- and @mappend as bs == [{a}, {b x-z}, {c g-m}, {d-f}, {n-w}]@.
--
-- Yet another example: @as = [{a-b x-z}, {c-w}]@, @bs = [{a-z}]@ and
-- @mappend as bs = as@. This is because @bs == mempty@.
--
module Core.Partition.Internal
       ( 
         Pa(..)
       , reversePa
       ) where

import Data.Monoid

data Pa code symb = Cons !code !symb (Pa code symb)
                  | Nil
                  deriving (Eq, Ord, Show)

instance (Eq code, Enum code, Bounded code, Ord symb, Bounded symb)
         => Monoid (Pa code symb) where
  -- Partition with one block.
  mempty = Cons minBound maxBound Nil

  -- Intersection of two partitions.
  mappend = intersect newDict Nil
    where
      intersect dict acc pss@(Cons c s ps) pss'@(Cons c' s' ps')
        = case compare s s' of
            LT -> intersect dict' (Cons k s  acc) ps  pss'
            GT -> intersect dict' (Cons k s' acc) pss ps'
            EQ -> intersect dict' (Cons k s' acc) ps  ps'
        where
          (k, dict')  = insertLookup (c, c') dict
      intersect _ acc _ _ = reversePa acc Nil

  -- Intersection of many partitions.
  mconcat = foldl mappend mempty

-- |Same as @reverse@ for lists, the second argument is accumulator.
reversePa :: Pa code symb -> Pa code symb -> Pa code symb
reversePa (Cons c s ps) acc = reversePa ps (Cons c s acc)
reversePa Nil           acc = acc

-- |Dictionary is used for translation of combined keys to normal keys.
type Dict code = ([((code, code), code)], code)

-- |Empty dictionary.
newDict :: Bounded code => Dict code
newDict = ([], minBound)

-- |Translates given combined key to normal key. Combined key is inserted to
-- the dictionary if it was not there.
insertLookup :: (Eq code, Enum code) => (code, code) -> Dict code -> (code, Dict code)
insertLookup k d@(dict, size)
  = case lookup k dict of
      Just v  -> (v, d)
      Nothing -> let d' = ((k, size):dict, succ size) in (size, d')
