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
         Symbol
       , BlockId
       , Pa
       , fromList
       , toList
       , getBlock
       , mergeWith
       , representatives
       , pmap
       , toIntervals
       -- * Symbol set

         -- | Symbol set is implemented in terms of partition.
         --   Symbol is in the set iff it is in the block with nonzero id.
       , Range(..)
       , fromRanges
       , toRanges
       , empty
       , alphabet
       , member
       , complement
       , union
       , intersect
       ) where

import Data.Monoid
import Core.Utils
import Data.Word (Word8)

-- ---------------------------------------------------------------------------
-- Symbol

-- | Partitions can be used only with symbols.
class (Bounded s, Enum s, Ord s) => Symbol s

instance Symbol Char
instance Symbol Word8

-- ---------------------------------------------------------------------------
-- Partition

-- | Identifier of blocks of partition.
type BlockId = Int

-- | Represents partition with numbered blocks parametrized by the type
--   of the symbol @s@.
data Pa s = Cons !BlockId !s (Pa s)
          | Nil
          deriving (Eq, Ord)

-- | Checks whether list can be converted to partition by 'fromList'.
check :: Symbol s => [(BlockId, s)] -> Bool
check [] = False
check [(_, s)] = s == maxBound
check ((b, s):(b', s'):xs) = b /= b' && s < s' && check ((b', s'):xs)

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
fromList :: Symbol s => [(BlockId, s)] -> Pa s
fromList pa
  | check pa  = fromL pa
  | otherwise = error "Core.Partition.fromList (PartitionL): bad input"
  where
    fromL ((b, s):xs) = Cons b s $ fromL xs
    fromL []          = Nil

-- | Converts partition to list. This is inverse of 'fromList'.
toList :: Pa s -> [(BlockId, s)]
toList (Cons b s xs) = (b, s):toList xs
toList Nil           = []

-- | @'getBlock' s pa@ returns id of the block which contains symbol @s@.
getBlock :: Symbol s => s -> Pa s -> BlockId
getBlock symbol (Cons b s xs)
  | symbol <= s = b
  | otherwise   = getBlock symbol xs
getBlock _ Nil = error "Core.Partition.getBlock (PartitionL): bad input"

-- | Merges consecutive blocks with same block id.
mergeConsecutive :: Pa s -> Pa s
mergeConsecutive (Cons block symbol ys) = merge block symbol ys
  where
    merge prevBlock prevSymbol (Cons b s xs)
      | prevBlock == b = merge b s xs
      | otherwise      = Cons prevBlock prevSymbol $ merge b s xs
    merge prevBlock prevSymbol Nil = Cons prevBlock prevSymbol Nil
mergeConsecutive Nil = error "mergeConsecutive: bad partition"

-- | Function @'mergeWith' f p p'@ takes two partitions @p@ and @p'@
--   and creates new partition such that
--
-- * if symbol @s@ is in block @b@ in partition @p@ and in block @b'@
--   in partition @p'@ then @s@ is in block @f b b'@ in the new partition.
mergeWith :: Symbol s
          => (BlockId -> BlockId -> BlockId) -> Pa s -> Pa s -> Pa s
mergeWith op p p' = mergeConsecutive $ merge p p'
  where
    merge pss@(Cons b s ps) pss'@(Cons b' s' ps')
      = case compare s s' of
          LT -> cont s  ps  pss'
          GT -> cont s' pss ps'
          EQ -> cont s  ps  ps'
      where
        newBlock = b `op` b'
        cont symb xss yss = Cons newBlock symb $ merge xss yss
    merge Nil Nil = Nil
    merge _ _  = error "Core.Partition.mergeWith (PartitionL): bad input"

-- | Function @'representatives' pa@ takes partition @pa@ and returns list.
--   For every block of @pa@ resulting list contains one pair with id
--   of that block and symbol from that block.
representatives :: Pa s -> [(BlockId, s)]
representatives = sortAndNubWith fst . toList

-- | Function @'pmap' f pa@ applies @f@ to the id of each block.
--
--   Blocks with the same id after transformation will be merged into one.
--   (happens only if @f@ is not injective)
pmap :: (BlockId -> BlockId) -> Pa s -> Pa s
pmap f = mergeConsecutive . map'
  where
    map' (Cons b s xs) = Cons (f b) s (map' xs)
    map' Nil           = Nil

-- | Converts partition to list of triples (block, first symbol, last symbol).
toIntervals :: Symbol s => Pa s -> [(BlockId, s, s)]
toIntervals = toIntervals' Nothing . toList
  where
    toIntervals' Nothing []
      = error "Core.Partition.toIntervals: bad partition"
    toIntervals' (Just _) [] = []
    toIntervals' Nothing ((b, s):xs)
      = (b, minBound, s) :toIntervals' (Just s) xs
    toIntervals' (Just prev) ((b, s):xs)
      = (b, succ prev, s):toIntervals' (Just s) xs

-- ---------------------------------------------------------------------------
-- Symbol set

-- | @Range a b@ contains every symbol @s@ which meets @a <= s <= b@.
--
--   Valid range has @a <= b@.
data Range s = Range !s !s
             deriving (Eq, Show)

-- | @'fromRanges' rs@ constructs symbol set with symbols from ranges
--   in the list @rs@.
fromRanges :: Symbol s => [Range s] -> Pa s
fromRanges = foldl union empty . map (fromList . rangeToList)
  where
    rangeToList (Range x y)
      | x == minBound = if y == maxBound then [(1, maxBound)]
                                         else [(1, y), (0, maxBound)]
      | y == maxBound = [(0, pred x), (1, maxBound)]
      | otherwise     = [(0, pred x), (1, y), (0, maxBound)]

-- | Returns ranges with characters inside symbol set.
toRanges :: Symbol s => Pa s -> [Range s]
toRanges = map intervalToRange . filterIntervals . toIntervals
  where
    filterIntervals             = filter (\(b, _, _) -> b /= 0)
    intervalToRange (_, lo, hi) = Range lo hi

-- | Empty symbol set.
empty :: Symbol s => Pa s
empty = fromList [(0, maxBound)]

-- | Symbol set with all symbols.
alphabet :: Symbol s => Pa s
alphabet = fromList [(1, maxBound)]

-- | Function @'member' s set@ returns whether symbol @s@ is in @set@.
member :: Symbol s => s -> Pa s -> Bool
member s = (/=0) . getBlock s

-- | Returns complement of the given set.
complement :: Pa s -> Pa s
complement = pmap (\b -> if b == 0 then 1 else 0)

-- | Union of symbol sets.
union :: Symbol s => Pa s -> Pa s -> Pa s
union = mergeWith (\a b -> if a == 0 && b == 0 then 0 else 1)

-- | Intersection of symbol sets.
intersect :: Symbol s => Pa s -> Pa s -> Pa s
intersect = mergeWith (\a b -> if a == 0 || b == 0 then 0 else 1)

-- ---------------------------------------------------------------------------
-- Pa is monoid with intersection

instance Symbol s => Monoid (Pa s) where
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
      isect _ _ _     = error "mappend (Pa): bad input"

      translate k dict@(ddata, size)
        = case lookup k ddata of
            Just newBlock -> (newBlock, dict)
            _             -> (size, ((k, size):ddata, succ size))

  mconcat = foldl mappend mempty
