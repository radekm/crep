{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances #-}

-- |
-- Module    : Core.Partition
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- This module implements:
--
-- * partition of the alphabet with numbered blocks
--   (i.e. unique identifier is associated with each block of the partition),
--
-- * set of symbols.
--
-- Partitions form commutative monoid with intersection.
-- Intersection of two partitions @p@ and @p'@ is defined as set
--
-- > {b /\ b' | b in p, b' in p', b /\ b' is nonempty}
--
-- Partitions with numbered blocks form commutative monoid too.
module Core.Partition
       (
         -- * Partition

         -- | Every partition is instance of @Pa@ and @Monoid@.
         Pa(..)
       , BlockId
       , PartitionL
         -- * Symbol set

         -- | Symbol set is implemented in terms of partition.
         --   Symbol is in the set iff it is in the block with nonzero id.
       , Range(..)
       , fromRanges
       , empty
       , alphabet
       , member
       , complement
       , union
       , intersect
       ) where

import Data.Monoid
import Core.Utils

-- ---------------------------------------------------------------------------
-- Partition

-- | Identifier of blocks of partition.
type BlockId = Int

-- | A @Pa p s@ instance represents partition with numbered blocks @p s@
--   parametrized by the type of the symbol @s@.
--
--   Complete definition consists of 'fromList', 'toList', 'getBlock',
--   'mergeWith'.
class (Ord s, Bounded s) => Pa p s where
  -- | Converts list to partition.
  --
  --   Input list @[(b_1, s_1), (b_2, s_2), ..., (b_n, s_n)]@ will be
  --   converted to partition where
  --
  -- * every symbol @s@ which meets @s_(i-1) < s <= s_i@ belongs
  --   to the block with id @b_i@.
  --
  --   Input list must meet following conditions:
  --
  -- * List is nonempty and last item in the list contains symbol @maxBound@.
  --
  -- * Each item contains symbol strictly greater than preceding item.
  --
  -- * Consecutive items differ in block id.
  fromList :: [(BlockId, s)] -> p s
  -- | Converts partition to list. This is inverse of 'fromList'.
  toList :: p s -> [(BlockId, s)]
  -- | @'getBlock' s pa@ returns id of the block which contains symbol @s@.
  getBlock :: s -> p s -> BlockId
  -- | Function @'mergeWith' f p p'@ takes two partitions @p@ and @p'@
  --   and creates new partition such that
  --
  -- * if symbol @s@ is in block @b@ in partition @p@ and in block @b'@
  --   in partition @p'@ then @s@ is in block @f b b'@ in the new partition.
  mergeWith :: (BlockId -> BlockId -> BlockId) -> p s -> p s -> p s
  -- | Function @'representatives' pa@ takes partition @pa@ and returns list.
  --   For every block of @pa@ resulting list contains one pair with id
  --   of that block and symbol from that block.
  representatives :: p s -> [(BlockId, s)]
  representatives = sortAndNubWith fst . toList
  -- | Function @'pmap' f pa@ applies @f@ to the id of each block.
  --
  --   Blocks with the same id after transformation will be merged into one.
  --   (happens only if @f@ is not injective)
  pmap :: (BlockId -> BlockId) -> p s -> p s
  pmap f = mergeWith (\_ a -> f a) empty

-- ---------------------------------------------------------------------------
-- Symbol set

-- | @Range a b@ contains every symbol @s@ which meets @a <= s <= b@.
--
--   Valid range has @a <= b@.
data Range s = Range !s !s

-- | @'fromRanges' rs@ constructs symbol set with symbols from ranges
--   in the list @rs@.
fromRanges :: (Enum s, Pa p s) => [Range s] -> p s
fromRanges = foldl union empty . map (fromList . rangeToList)
  where
    rangeToList (Range x y)
      | x == minBound = if y == maxBound then [(1, maxBound)]
                                         else [(1, y), (0, maxBound)]
      | y == maxBound = [(0, pred x), (1, maxBound)]
      | otherwise     = [(0, pred x), (1, y), (0, maxBound)]

-- | Empty symbol set.
empty :: Pa p s => p s
empty = fromList [(0, maxBound)]

-- | Symbol set with all symbols.
alphabet :: Pa p s => p s
alphabet = fromList [(1, maxBound)]

-- | Function @'member' s set@ returns whether symbol @s@ is in @set@.
member :: Pa p s => s -> p s -> Bool
member s = (/=0) . getBlock s

-- | Returns complement of the given set.
complement :: Pa p s => p s -> p s
complement = pmap (\b -> if b == 0 then 1 else 0)

-- | Union of symbol sets.
union :: Pa p s => p s -> p s -> p s
union = mergeWith (\a b -> if a == 0 && b == 0 then 0 else 1)

-- | Intersection of symbol sets.
intersect :: Pa p s => p s -> p s -> p s
intersect = mergeWith (\a b -> if a == 0 || b == 0 then 0 else 1)

-- ---------------------------------------------------------------------------
-- PartitionL - definition and instances

-- | Represents partition of the alphabet with symbols of type @s@.
data PartitionL s = Cons !BlockId !s (PartitionL s)
                  | Nil
                  deriving (Eq, Ord)

-- | Checks whether list can be converted to partition by 'fromList'.
check :: (Ord s, Bounded s) => [(BlockId, s)] -> Bool
check [] = False
check [(_, s)] = s == maxBound
check ((b, s):(b', s'):xs) = b /= b' && s < s' && check ((b', s'):xs)

instance (Ord s, Bounded s) => Pa PartitionL s where
  fromList pa
    | check pa  = fromL pa
    | otherwise = error "Core.Partition.fromList (PartitionL): bad input"
    where
      fromL ((b, s):xs) = Cons b s $ fromL xs
      fromL []          = Nil

  toList (Cons b s xs) = (b, s):toList xs
  toList Nil           = []

  getBlock symbol (Cons b s xs)
    | symbol <= s = b
    | otherwise   = getBlock symbol xs
  getBlock _ Nil = error "Core.Partition.getBlock (PartitionL): bad input"

  mergeWith op xss'@(Cons block _ _) yss'@(Cons block' _ _)
    = merge (block `op` block') undefined {- prevSymb can be any value -}
            xss' yss'
    where
      merge prevBlock prevSymb pss@(Cons b s ps) pss'@(Cons b' s' ps')
        = case compare s s' of
            LT -> cont s  ps  pss'
            GT -> cont s' pss ps'
            EQ -> cont s  ps  ps'
        where
          newBlock = b `op` b'
          cont symb xss yss
            | newBlock == prevBlock = merge prevBlock symb xss yss
            | otherwise = Cons prevBlock prevSymb
                               (merge newBlock symb xss yss)
      merge prevBlock prevSymb Nil Nil
        = Cons prevBlock prevSymb Nil
      merge _ _ _ _
        = error "Core.Partition.mergeWith (PartitionL): bad input"
  mergeWith _ _ _
    = error "Core.Partition.mergeWith (PartitionL): no intervals"

  pmap _ Nil = error "Core.Partition.pmap (PartitionL): bad input"
  pmap f (Cons b' s' xs') = map' b' s' xs'
    where
      map' prevBlock prevSymb (Cons b s xs)
        | prevBlock /= curBlock = Cons prevBlock prevSymb (map' curBlock s xs)
        | otherwise             = map' prevBlock s xs  -- Blocks are merged.
        where
          curBlock = f b
      map' prevBlock prevSymb Nil = Cons prevBlock prevSymb Nil

instance (Ord s, Bounded s) => Monoid (PartitionL s) where
  mempty = empty

  mappend = isect ([], 0)
    where
      isect dict xss@(Cons b s xs) yss@(Cons b' s' ys)
        = case compare s s' of
            LT -> Cons newBlock s  (isect newDict xs  yss)
            GT -> Cons newBlock s' (isect newDict xss ys)
            EQ -> Cons newBlock s  (isect newDict xs  ys)
        where
          (newBlock, newDict) = translate (b, b') dict
      isect _ Nil Nil = Nil
      isect _ _ _     = error "mappend (PartitionL): bad input"

      translate k dict@(ddata, size)
        = case lookup k ddata of
            Just newBlock -> (newBlock, dict)
            _             -> (size, ((k, size):ddata, succ size))

  mconcat = foldl mappend mempty
