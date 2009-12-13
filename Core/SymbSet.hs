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

  -- |Returns the complement of the given symbol set.
  complement :: SymbSet a -> SymbSet a

-- |The type @'CharSet'@ represents the set of characters. It's an type alias
-- for @'SymbSet' Char@
type CharSet = SymbSet Char

moduleError :: String -> String -> a
moduleError fun msg = error ("Core.SymbSet." ++ fun ++ ':':' ':msg)


instance Symbol Char where
  newtype SymbSet Char = S (List (Pair Char Char))

  newtype Range Char = R (Pair Char Char)
                     deriving (Eq, Ord)

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

instance Show (Range Char) where
  showsPrec _ (R range)
    | a == b    = (++) (esc a)
    | otherwise = (++) (esc a ++ '-':esc b)
    where
      (a, b)  = fromPair range
      esc c | c `elem` "[]-^"  = '\\':c:""
      esc c = escapeSpecial c

instance Show (SymbSet Char) where
  showsPrec _ (S ranges)
    = ('[':) . foldl (.) id xs . (']':)
    where
      xs :: [String -> String]
      xs = intersperse (' ':) $ map (shows . R) $ toList ranges

