-- |
-- Module    : Core.Partition
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- This module implements  partition of the alphabet with numbered blocks
-- (i.e. unique identifier is associated with each block of the partition).
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
  | otherwise = moduleError "fromList" "bad input"
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
getBlock _ Nil = moduleError "getBlock" "bad partition"

-- | Merges consecutive blocks with same block id.
mergeConsecutive :: Pa s -> Pa s
mergeConsecutive (Cons block symbol ys) = merge block symbol ys
  where
    merge prevBlock prevSymbol (Cons b s xs)
      | prevBlock == b = merge b s xs
      | otherwise      = Cons prevBlock prevSymbol $ merge b s xs
    merge prevBlock prevSymbol Nil = Cons prevBlock prevSymbol Nil
mergeConsecutive Nil = moduleError "mergeConsecutive" "bad partition"

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
        cont symbol xss yss = Cons newBlock symbol $ merge xss yss
    merge Nil Nil = Nil
    merge _ _  = moduleError "mergeWith" "bad partition"

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
      = moduleError "toIntervals" "bad partition"
    toIntervals' (Just _) [] = []
    toIntervals' Nothing ((b, s):xs)
      = (b, minBound, s) :toIntervals' (Just s) xs
    toIntervals' (Just prev) ((b, s):xs)
      = (b, succ prev, s):toIntervals' (Just s) xs

-- ---------------------------------------------------------------------------
-- Pa is monoid with intersection

instance Symbol s => Monoid (Pa s) where
  mempty = Cons 0 maxBound Nil

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
      isect _ _ _     = error "mappend (Pa): bad partition"

      translate k dict@(ddata, size)
        = case lookup k ddata of
            Just newBlock -> (newBlock, dict)
            _             -> (size, ((k, size):ddata, succ size))

  mconcat = foldl mappend mempty

-- ---------------------------------------------------------------------------

moduleError :: String -> String -> a
moduleError fun msg = error $ "Core.Partition." ++ fun ++ ": " ++ msg
