{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module    : Core.SymbSet
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Set of symbols and range of symbols.
--
module Core.SymbSet
  ( SymbSet
  , CharSet
  , ByteSet
  , Range
  , mkRange
  , empty
  , alphabet
  , fromRanges
  , toPartition
  , fromPartition
  , member
  , firstSymb
  , complement
  , union
  , intersect
  ) where

import Data.List (intersperse, sortBy, groupBy)
import Data.Bits ((.|.), (.&.))
import Data.Word (Word64, Word8, Word)
import Core.Utils
import Core.Partition.Internal

class Symbol a where
  -- |Set of symbols.
  data SymbSet a
    
  -- |Range of symbols.
  data Range a

  -- |(@'mkRange' a b@) creates range containing symbols from @a@ to @b@.
  --
  -- Error is called when @a > b@.
  mkRange :: a -> a -> Range a

  -- |Empty set of symbols.
  empty :: SymbSet a

  -- |Set with all symbols.
  alphabet :: SymbSet a

  -- |The function (@'fromRanges' rs@) creates new symbol set containing
  -- symbols from ranges in the given list @rs@.
  fromRanges :: [Range a] -> SymbSet a

  -- |Converts symbol set to partition with two blocks (symbols from original
  -- set and symbols from its complement).
  toPartition :: SymbSet a -> Pa a

  -- |Creates symbol sets from partition. For each block of partition
  -- one symbol set is created.
  fromPartition :: Pa a -> [SymbSet a]

  -- |(@'member' x xs@) returns whether @x@ is element of the set @xs@.
  member :: a -> SymbSet a -> Bool

  -- |Returns the first symbol which is member of given symbol set.
  --
  -- Error is called when symbol set contains no symbols.
  firstSymb :: SymbSet a -> a

  -- |Returns the complement of the given symbol set.
  complement :: SymbSet a -> SymbSet a

  -- |Returns union of given symbol sets.
  union :: SymbSet a -> SymbSet a -> SymbSet a
  
  -- |Returns intersection of given symbol sets.
  intersect :: SymbSet a -> SymbSet a -> SymbSet a

-- |The type @'CharSet'@ represents the set of characters. It's type synonym
-- for @'SymbSet' Char@.
type CharSet = SymbSet Char

-- |The type @'ByteSet'@ represents the set of bytes. It's type synonym
-- for @'SymbSet' Word8@.
type ByteSet = SymbSet Word8

moduleError :: String -> String -> a
moduleError fun msg = error ("Core.SymbSet." ++ fun ++ ':':' ':msg)


instance Symbol Char where
  -- Set of characters is represented as a partition where intervals with
  -- characters inside the set have key @oneC@ intervals with characters
  -- inside the set's complement have key 0.
  newtype SymbSet Char = SC (Pa Char)
                       deriving (Eq, Ord)

  data Range Char = RC {-# UNPACK #-} !Char {-# UNPACK #-} !Char
                  deriving (Eq, Ord)

  empty = SC $ PC (toValueC maxBound) NilC

  alphabet = SC $ PC (toValueC maxBound .|. oneC) NilC

  mkRange a b
    | a <= b    = RC a b
    | otherwise = moduleError "mkRange" "invalid range"

  fromRanges ranges = foldl union empty (map rangeToSet ranges)
    where
      -- Converts range to character set.
      rangeToSet (RC a b)
        | a == minBound = if b == maxBound then alphabet
                                           else SC $ PC inside (PC after NilC)
        | b == maxBound = SC $ PC before (PC inside NilC)
        | otherwise     = SC $ PC before (PC inside (PC after NilC))
        where
          before = toValueC $ pred a 
          after  = toValueC maxBound
          inside = toValueC b .|. oneC

  fromPartition
    = map (SC . toSymbSet NilC)
        -- Group triples by key.
        . groupTriples . sortTriples
        -- Each interval from the partition is converted to the triple
        -- @(key, lastV, value)@ where @lastV@ is the value from the previous
        -- interval.
        --
        -- First interval does not have @lastV@ and so we use @Nothing@.
        . toTriples Nothing
    where
      toTriples lastV (PC p ps) = (keyC p, lastV, v):toTriples (Just v) ps
        where
          v = valueC p
      toTriples _ NilC = []

      sortTriples = sortBy (\(a, _, _) (b, _, _) -> compare a b)
      groupTriples = groupBy (\(a, _, _) (b, _, _) -> a == b)

      -- Takes list of triples and creates partition.
      toSymbSet :: Pa Char -> [(Word64, Maybe Word64, Word64)] -> Pa Char
      toSymbSet acc ((_, Just a, b):ts)
        = toSymbSet (PC (b .|. oneC) (PC a acc)) ts
      toSymbSet acc ((_, _, b):ts)
        = toSymbSet (PC (b .|. oneC) acc) ts
      toSymbSet acc@(PC lastV _) []
        -- Add maxChar to the end.
        | lastV /= maxChar = reverseC acc (PC maxChar NilC)
        | otherwise        = reverseC acc NilC
        where
          maxChar = toValueC maxBound
      toSymbSet _ _ = moduleError "toSymbSet" "no intervals"

  toPartition (SC pa) = pa

  member c (SC pa) = isInside pa
    where
      c' = toValueC c
      isInside (PC p ps)
        -- Character belongs to the current interval.
        | c' <= valueC p = keyC p /= 0
        | otherwise      = isInside ps
      isInside _ = moduleError "isInside" "invalid set"

  firstSymb (SC (PC p _))
    | keyC p /= 0     = minBound
    | val /= maxBound = succ val
    | otherwise       = moduleError "firstSymb" "empty set"
    where
      val = fromValueC $ valueC p
  firstSymb _ = moduleError "firstSymb" "no intervals"

  complement (SC pa) = SC $ compl pa
    where
      compl (PC p ps)
        | keyC p /= 0 = PC (valueC p) $ compl ps
        | otherwise   = PC (p .|. oneC) $ compl ps
      compl NilC = NilC

  union (SC as) (SC bs) = SC $ mergeC (.|.) as bs

  intersect (SC as) (SC bs) = SC $ mergeC (.&.) as bs

mergeC :: (Word64 -> Word64 -> Word64) -> Pa Char -> Pa Char -> Pa Char
mergeC op xss@(PC x _) yss@(PC y _)
  = merge (keyC x `op` keyC y) 0 {- any value -} NilC xss yss
  where
    merge lastK lastV acc ass@(PC a as) bss@(PC b bs)
      = case compare a' b' of
          LT -> cont a' as  bss
          GT -> cont b' ass bs
          EQ -> cont a' as  bs
      where
        a' = valueC a
        b' = valueC b
        k = keyC a `op` keyC b
        cont v
          | k == lastK = merge lastK v acc
          | otherwise  = merge k v (PC (lastV .|. lastK) acc)
    merge lastV lastK acc _ _ = reverseC acc (PC (lastV .|. lastK) NilC)
mergeC _ _ _ = moduleError "mergeC" "no intervals"

instance Show (Range Char) where
  showsPrec _ (RC a b)
    | a == b    = (++) (esc a)
    | otherwise = (++) (esc a ++ '-':esc b)
    where
      esc c | c `elem` "]-^# " = '\\':c:""
      esc c = escapeSpecial c

instance Show (SymbSet Char) where
  showsPrec _ (SC pa)
    = ('[':) . foldl (.) id xs . (']':)
    where
      xs :: [String -> String]
      xs = intersperse (' ':) $ map shows (toRanges Nothing pa)

      -- Convert partition to ranges.
      toRanges lastC (PC p ps)
        | keyC p /= 0 = RC prevC curC:toRanges (Just curC) ps
        | otherwise   =               toRanges (Just curC) ps
        where
          prevC = case lastC of { Just c -> succ c ; _ -> minBound }
          curC  = fromValueC $ valueC p
      toRanges _ _ = []


-- The code which follows is analagous to the previous code.

instance Symbol Word8 where
  newtype SymbSet Word8 = SB (Pa Word8)
                        deriving (Eq, Ord)

  data Range Word8 = RB {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                   deriving (Eq, Ord)

  empty = SB $ PB (toValueB maxBound) NilB

  alphabet = SB $ PB (toValueB maxBound .|. oneB) NilB

  mkRange a b
    | a <= b    = RB a b
    | otherwise = moduleError "mkRange" "invalid range"

  fromRanges ranges = foldl union empty (map rangeToSet ranges)
    where
      -- Converts range to character set.
      rangeToSet (RB a b)
        | a == minBound = if b == maxBound then alphabet
                                           else SB $ PB inside (PB after NilB)
        | b == maxBound = SB $ PB before (PB inside NilB)
        | otherwise     = SB $ PB before (PB inside (PB after NilB))
        where
          before = toValueB $ pred a 
          after  = toValueB maxBound
          inside = toValueB b .|. oneB

  fromPartition
    = map (SB . toSymbSet NilB)
        -- Group triples by key.
        . groupTriples . sortTriples
        -- Each interval from the partition is converted to the triple
        -- @(key, lastV, value)@ where @lastV@ is the value from the previous
        -- interval.
        --
        -- First interval does not have @lastV@ and so we use @Nothing@.
        . toTriples Nothing
    where
      toTriples lastV (PB p ps) = (keyB p, lastV, v):toTriples (Just v) ps
        where
          v = valueB p
      toTriples _ NilB = []

      sortTriples = sortBy (\(a, _, _) (b, _, _) -> compare a b)
      groupTriples = groupBy (\(a, _, _) (b, _, _) -> a == b)

      -- Takes list of triples and creates partition.
      toSymbSet :: Pa Word8 -> [(Word, Maybe Word, Word)] -> Pa Word8
      toSymbSet acc ((_, Just a, b):ts)
        = toSymbSet (PB (b .|. oneB) (PB a acc)) ts
      toSymbSet acc ((_, _, b):ts)
        = toSymbSet (PB (b .|. oneB) acc) ts
      toSymbSet acc@(PB lastV _) []
        -- Add maxChar to the end.
        | lastV /= maxByte = reverseB acc (PB maxByte NilB)
        | otherwise        = reverseB acc NilB
        where
          maxByte = toValueB maxBound
      toSymbSet _ _ = moduleError "toSymbSet" "no intervals"

  toPartition (SB pa) = pa

  member c (SB pa) = isInside pa
    where
      c' = toValueB c
      isInside (PB p ps)
        -- Byte belongs to the current interval.
        | c' <= valueB p = keyB p /= 0
        | otherwise      = isInside ps
      isInside _ = moduleError "isInside" "invalid set"

  firstSymb (SB (PB p _))
    | keyB p /= 0     = minBound
    | val /= maxBound = succ val
    | otherwise       = moduleError "firstSymb" "empty set"
    where
      val = fromValueB $ valueB p
  firstSymb _ = moduleError "firstSymb" "no intervals"

  complement (SB pa) = SB $ compl pa
    where
      compl (PB p ps)
        | keyB p /= 0 = PB (valueB p) $ compl ps
        | otherwise   = PB (p .|. oneB) $ compl ps
      compl NilB = NilB

  union (SB as) (SB bs) = SB $ mergeB (.|.) as bs

  intersect (SB as) (SB bs) = SB $ mergeB (.&.) as bs

mergeB :: (Word -> Word -> Word) -> Pa Word8 -> Pa Word8 -> Pa Word8
mergeB op xss@(PB x _) yss@(PB y _)
  = merge (keyB x `op` keyB y) 0 {- any value -} NilB xss yss
  where
    merge lastK lastV acc ass@(PB a as) bss@(PB b bs)
      = case compare a' b' of
          LT -> cont a' as  bss
          GT -> cont b' ass bs
          EQ -> cont a' as  bs
      where
        a' = valueB a
        b' = valueB b
        k = keyB a `op` keyB b
        cont v
          | k == lastK = merge lastK v acc
          | otherwise  = merge k v (PB (lastV .|. lastK) acc)
    merge lastV lastK acc _ _ = reverseB acc (PB (lastV .|. lastK) NilB)
mergeB _ _ _ = moduleError "mergeB" "no intervals"

instance Show (Range Word8) where
  showsPrec x (RB a b) = showsPrec x $ RC (toEnum $ fromIntegral a)
                                          (toEnum $ fromIntegral b)

instance Show (SymbSet Word8) where
  showsPrec x (SB pa) = showsPrec x $ SC $ toCharSet pa
    where
      -- ByteSet to CharSet.
      toCharSet (PB p ps)
        | keyB p == 0 = PC valC            (toCharSet ps)
        | otherwise   = PC (oneC .|. valC) (toCharSet ps)
        where
          valC = fromIntegral $ valueB p
      toCharSet NilB = NilC

