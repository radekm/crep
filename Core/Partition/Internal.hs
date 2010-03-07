{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

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
  ( Pa(..)
  , reverseC
  , oneC
  , keyC
  , valueC
  , toValueC
  , fromValueC
  , reverseB
  , oneB
  , keyB
  , valueB
  , toValueB
  , fromValueB
  ) where

import Data.Monoid
import Data.Word (Word64, Word8, Word)
import Data.Bits ((.|.), (.&.), complement, shiftL)

-- |@'Pa' e@ represents partition of values of type @e@.
data family Pa :: * -> *

-- |Partitioning of the Unicode code points.
--
-- Partition is represented by the list of intervals. Interval is a pair
-- @(key, value)@ where @key@ identifies a block of partition and @value@
-- is the last character in the interval. When two intervals have equal keys,
-- they belong to the same block of partition.
--
-- Example: Partitition @[{a-e n-z}, {f-m}]@ consists of three intervals
-- a-e, f-m, n-z and it will be represented by @[(0, e), (1, m), (0, z)]@.
-- You can see that intervals are sorted by their value and two consecutive
-- intervals have different key. Each interval starts where the previous
-- ends.
--
-- Another example: Partition @[{a-b c-d f-g h-z}, {e}]@ consists of three
-- intervals a-d, e, f-z. This partition will be represented
-- by @[(0, d), (1, e), (0, z)]@.
--
-- For better performance we store each interval as 64-bit word. Lowest
-- 21 bits are for the value (character), next 21 bits are for the key.
data instance Pa Char = PC {-# UNPACK #-} !Word64 (Pa Char)
                      | NilC
                      deriving (Eq, Ord, Show)

instance Monoid (Pa Char) where
  -- Partition with one block.
  mempty = PC (toValueC maxBound) NilC

  -- Intersection of two partitions.
  mappend = intersect dictC NilC
    where
      intersect dict acc ass@(PC a as) bss@(PC b bs)
        = case compare x y of
            LT -> intersect dict' (PC (k .|. x) acc) as  bss
            GT -> intersect dict' (PC (k .|. y) acc) ass bs
            EQ -> intersect dict' (PC (k .|. y) acc) as  bs
        where
          x = valueC a
          y = valueC b
          combinedKey = (shiftC $ keyC a) .|. (keyC b)
          (k, dict')  = insertLookupC combinedKey dict
      intersect _ acc _ _ = reverseC acc NilC

  -- Intersection of many partitions.
  mconcat = foldl mappend mempty

-- |Same as @reverse@ for lists, the second argument is accumulator.
reverseC :: Pa Char -> Pa Char -> Pa Char
reverseC (PC p ps) acc = reverseC ps (PC p acc)
reverseC NilC acc      = acc

-- |Converts value to character.
fromValueC :: Word64 -> Char
fromValueC = toEnum . fromIntegral

-- |Converts character to value.
toValueC :: Char -> Word64
toValueC = fromIntegral . fromEnum

-- |Returns key.
keyC :: Word64 -> Word64
keyC = (.&. (complement $ oneC - 1))

-- |Returns value.
valueC :: Word64 -> Word64
valueC = (.&. (oneC - 1))

-- |Left shift by the bit length of value.
shiftC :: Word64 -> Word64
shiftC = (`shiftL` 21)

-- |Lowest non-zero key.
--
-- That is the lowest number @n@ where @keyC n /= 0@.
oneC :: Word64
oneC = shiftC 1

-- |Dictionary is used for translation of combined keys to normal keys.
type DictC = ([(Word64, Word64)], Word64)

-- |Empty dictionary.
dictC :: DictC
dictC = ([], 0)

-- |Translates given combined key to normal key. Combined key is inserted to
-- the dictionary if it was not there.
insertLookupC :: Word64 -> DictC -> (Word64, DictC)
insertLookupC k d@(dict, size)
  = case lookup k dict of
      Just v  -> (v, d)
      Nothing -> let d' = ((k, size):dict, size+oneC) in (size, d')

data instance Pa Word8 = PB {-# UNPACK #-} !Word (Pa Word8)
                       | NilB
                       deriving (Eq, Ord, Show)

instance Monoid (Pa Word8) where
  -- Partition with one block.
  mempty = PB (toValueB maxBound) NilB

  -- Intersection of two partitions.
  mappend = intersect dictB NilB
    where
      intersect dict acc ass@(PB a as) bss@(PB b bs)
        = case compare x y of
            LT -> intersect dict' (PB (k .|. x) acc) as  bss
            GT -> intersect dict' (PB (k .|. y) acc) ass bs
            EQ -> intersect dict' (PB (k .|. y) acc) as  bs
        where
          x = valueB a
          y = valueB b
          combinedKey = (shiftB $ keyB a) .|. (keyB b)
          (k, dict')  = insertLookupB combinedKey dict
      intersect _ acc _ _ = reverseB acc NilB

  -- Intersection of many partitions.
  mconcat = foldl mappend mempty

-- |Same as @reverse@ for lists, the second argument is accumulator.
reverseB :: Pa Word8 -> Pa Word8 -> Pa Word8
reverseB (PB p ps) acc = reverseB ps (PB p acc)
reverseB NilB acc      = acc

-- |Converts value to byte.
fromValueB :: Word -> Word8
fromValueB = fromIntegral

-- |Converts byte to value.
toValueB :: Word8 -> Word
toValueB = fromIntegral

-- |Returns key.
keyB :: Word -> Word
keyB = (.&. (complement $ oneB - 1))

-- |Returns value.
valueB :: Word -> Word
valueB = (.&. (oneB - 1))

-- |Left shift by the bit length of value.
shiftB :: Word -> Word
shiftB = (`shiftL` 8)

-- |Lowest non-zero key.
--
-- That is the lowest number @n@ where @keyB n /= 0@.
oneB :: Word
oneB = shiftB 1

-- |Dictionary is used for translation of combined keys to normal keys.
type DictB = ([(Word, Word)], Word)

-- |Empty dictionary.
dictB :: DictB
dictB = ([], 0)

-- |Translates given combined key to normal key. Combined key is inserted to
-- the dictionary if it was not there.
insertLookupB :: Word -> DictB -> (Word, DictB)
insertLookupB k d@(dict, size)
  = case lookup k dict of
      Just v  -> (v, d)
      Nothing -> let d' = ((k, size):dict, size+oneB) in (size, d')

