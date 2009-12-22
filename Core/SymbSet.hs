{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module    : Core.SymbSet
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Set of symbols and range of symbols.
--
module Core.SymbSet where

import Data.List (intersperse, sort)

import qualified Data.Adaptive.List as L
import qualified Data.Adaptive.Tuple as T
import Data.Adaptive.List (List, cons, fromList, toList)
import Data.Adaptive.Tuple (Pair, fromPair, pair)


import Core.Utils

class Symbol a where
  -- |Set of symbols.
  data SymbSet a
    
  -- |Range of symbols.
  data Range a

  -- |The function (@'mkRange' a b@) creates range containing symbols
  -- from @a@ to @b@.
  --
  -- Error is thrown when @a > b@.
  mkRange :: a -> a -> Range a

  -- | Empty sybmol set.
  empty :: SymbSet a

  -- |The function (@'fromRanges' rs@) creates new symbol set containing
  -- given ranges @rs@.
  fromRanges :: [Range a] -> SymbSet a

  -- |The function (@'member' x xs@) returns whether @x@ is element
  -- of the set @xs@.
  member :: a -> SymbSet a -> Bool

  -- |Returns the complement of the given symbol set.
  complement :: SymbSet a -> SymbSet a

  -- |Returns union of given symbol sets.
  union :: SymbSet a -> SymbSet a -> SymbSet a
  
  -- Returns intersection of given symbol sets.
  intersect :: SymbSet a -> SymbSet a -> SymbSet a

-- |The type @'CharSet'@ represents the set of characters. It's an type alias
-- for @'SymbSet' Char@
type CharSet = SymbSet Char

moduleError :: String -> String -> a
moduleError fun msg = error ("Core.SymbSet." ++ fun ++ ':':' ':msg)


instance Symbol Char where
  newtype SymbSet Char = S (List (Pair Char Char))
                       deriving Eq

  newtype Range Char = R (Pair Char Char)
                     deriving Eq

  empty = S (L.empty)

  mkRange a b
    | a <= b    = R (pair a b)
    | otherwise = moduleError "mkRange" "invalid range"

  fromRanges []         = empty
  fromRanges ranges = S $ L.reverse $ cons (pair l1 l2) mergedRngsNoLastRev
    where
      (R firstRange:rs) = sort ranges
      (f1, f2)          = fromPair firstRange
      -- @mergedRngsNoLastRev@ contains all merged ranges but last. The order
      -- is reversed.
      (l1, l2, mergedRngsNoLastRev) = foldl merge (f1, f2, L.empty) rs
      -- Combines neighbouring and overlapping ranges together.
      merge (p1, p2, merged) (R curRange)
        -- Ranges cannot be merged and so @p@ is finished.
        | p2 < c1 && succ p2 /= c1 = (c1, c2, cons (pair p1 p2) merged)
        -- Ranges overlap or are neighbours, we merge them.
        | otherwise                = (p1, max p2 c2, merged)
        where
          (c1, c2) = fromPair curRange

  member c (S ranges) = L.any isInsideRange ranges
    where
      isInsideRange :: Pair Char Char -> Bool
      isInsideRange range = T.fst range <= c && T.snd range >= c

  complement (S origRanges)
    -- The original set is empty and so its complement contains all symbols.
    | L.null origRanges = S $ fromList [pair minBound maxBound]
    -- The original set is not empty.
    | otherwise = S $ addMinSymbolRng $ L.reverse $ addMaxSymbolRng
                    $ complNoMinMaxSymbolRev
    where
      (firstSymbol, lastSymbolFromFirstRange) = fromPair $ L.head origRanges
      -- @complNoLowestHighestSymbolRev@ is the complement of the original set
      -- but without the ranges with the minimal and the maximal symbols.
      -- The order of the ranges is reversed.
      (lastSymbol, complNoMinMaxSymbolRev)
        = L.foldl func (lastSymbolFromFirstRange, L.empty) (L.tail origRanges)
        where
          func (lastSymbFromPrevRange, ranges) curRange
            = (T.snd curRange, cons (pair (succ lastSymbFromPrevRange)
                                          (pred $ T.fst curRange))
                                    ranges)
      -- Adds range with the min (max) symbol to the complement if such
      -- range should be there.
      addMinSymbolRng
        | firstSymbol == minBound = id
        | otherwise               = cons (pair minBound (pred firstSymbol))
      addMaxSymbolRng
        | lastSymbol == maxBound = id
        | otherwise              = cons (pair (succ lastSymbol) maxBound)

  union (S rangesA) (S rangesB) = S (merge rangesA rangesB L.empty)
    where
      merge as bs acc
        | L.null as = L.reverse acc `pp` bs
        | L.null bs = L.reverse acc `pp` as
        | a1 > b1   = merge bs as acc
        -- @a@ ends before @b@ starts.
        | a2 < b1 = if succ a2 == b1
                      -- @a@ is neighbour of @b@.
                      then merge (cons (pair a1 b2) bs') as' acc
                      -- @a@ cannot be extended so add it into accumulator.
                      else merge as' bs (cons a acc)
        -- @a@ and @b@ overlap, range @(a1, max a2 b2)@ will be in the union
        -- (it can be there as a subrange of some range).
        | otherwise = if a2 < b2
                        then merge (cons (pair a1 b2) bs') as' acc
                        else merge as bs' acc
        where
          Just (a, as') = L.uncons as
          Just (b, bs') = L.uncons bs
          (a1, a2) = fromPair a
          (b1, b2) = fromPair b

  intersect (S rangesA) (S rangesB) = S (merge rangesA rangesB L.empty)
    where
      merge as bs acc
        | L.null as || L.null bs = L.reverse acc
        | a1 > b1 = merge bs as acc
        -- No intersection of @a@ and @b@ since @a@ ends before @b@ starts.
        | a2 < b1 = merge as' bs acc
        -- @a@ and @b@ overlap, we add range @(b1, min a2 b2)@ to accumulator.
        | otherwise = if b2 < a2
                        then merge as bs' (cons b acc)
                        else merge bs as' (cons (pair b1 a2) acc)
        where
          Just (a, as') = L.uncons as
          Just (b, bs') = L.uncons bs
          (a1, a2) = fromPair a
          (b1, b2) = fromPair b

-- (++) operator for adaptive list has bug (infinite recursion).
pp xs ys
  | L.null xs = ys
  | otherwise = L.head xs `cons` (L.tail xs `pp` ys)

-- We cannot derive instance from @'Pair'@ because
-- @pair 'a' 'a' < pair 'b' 'b' == False@ and we need lexicographical order.
instance Ord (Range Char) where
  compare (R a) (R b) = fromPair a `compare` fromPair b

instance Show (Range Char) where
  showsPrec _ (R range)
    | a == b    = (++) (esc a)
    | otherwise = (++) (esc a ++ '-':esc b)
    where
      (a, b)  = fromPair range
      esc c | c `elem` "]-^# " = '\\':c:""
      esc c = escapeSpecial c

instance Show (SymbSet Char) where
  showsPrec _ (S ranges)
    = ('[':) . foldl (.) id xs . (']':)
    where
      xs :: [String -> String]
      xs = intersperse (' ':) $ map (shows . R) $ toList ranges

