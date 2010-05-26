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
       ( 
         SymbSet
       , CharSet
       , ByteSet
       , Range
       , mkRange
       , loR
       , hiR
       , empty
       , alphabet
       , fromRanges
       , toRanges
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
import Data.Word (Word, Word8)
import Core.Utils
import Core.Partition.Internal

-- |The type @'CharSet'@ represents the set of characters. It's type synonym
-- for @'SymbSet' Char@.
type CharSet = SymbSet Char

-- |The type @'ByteSet'@ represents the set of bytes. It's type synonym
-- for @'SymbSet' Word8@.
type ByteSet = SymbSet Word8

moduleError :: String -> String -> a
moduleError fun msg = error ("Core.SymbSet." ++ fun ++ ':':' ':msg)



-- Set of characters is represented as a partition where intervals with
-- characters inside the set have key @oneC@ intervals with characters
-- inside the set's complement have key 0.
newtype SymbSet a = S (Pa Word a)
                  deriving (Eq, Ord)

data Range a = R !a !a
             deriving (Eq, Ord)

empty :: Bounded a => SymbSet a
empty = S $ Cons 0 maxBound Nil

alphabet :: Bounded a => SymbSet a
alphabet = S $ Cons 1 maxBound Nil

mkRange :: Ord a => a -> a -> Range a
mkRange a b
  | a <= b    = R a b
  | otherwise = moduleError "mkRange" "invalid range"

loR :: Range a -> a
loR (R a _) = a

hiR :: Range a -> a
hiR (R _ b) = b

fromRanges :: (Ord a, Bounded a, Enum a) => [Range a] -> SymbSet a
fromRanges ranges = foldl union empty (map rangeToSet ranges)
  where
    -- Converts range to character set.
    rangeToSet (R a b)
      | a == minBound = if b == maxBound
                        then alphabet
                        else S              $ Cons 1 b $ Cons 0 maxBound Nil
      | b == maxBound = S $ Cons 0 (pred a)            $ Cons 1 maxBound Nil
      | otherwise     = S $ Cons 0 (pred a) $ Cons 1 b $ Cons 0 maxBound Nil

toRanges :: (Bounded a, Enum a) => SymbSet a -> [Range a]
toRanges (S pa) = convert Nothing pa
  where
    convert prevSymbol (Cons code symb ps)
      | code /= 0 = R start symb:convert (Just symb) ps
      | otherwise =              convert (Just symb) ps
      where
        start = case prevSymbol of { Just c -> succ c ; _ -> minBound }
    convert _ _ = []
  
fromPartition :: (Eq a, Bounded a, Ord code) =>  Pa code a -> [SymbSet a]
fromPartition
  = map (S . toSymbSet Nil)
    -- Group triples by key.
    . groupTriples . sortTriples
    -- Each interval from the partition is converted to the triple
    -- @(code, prevS, value)@ where @prevS@ is the symbol from the previous
    -- interval.
    --
    -- First interval does not have @prevS@ and so we use @Nothing@.
    . toTriples Nothing
  where
    toTriples prevS (Cons c s ps) = (c, prevS, s):toTriples (Just s) ps
    toTriples _     Nil           = []

    sortTriples  = sortBy  (\(a, _, _) (b, _, _) -> compare a b)
    groupTriples = groupBy (\(a, _, _) (b, _, _) -> a == b)

    -- Takes list of triples and creates partition.
    toSymbSet :: (Eq a, Bounded a)
              =>  Pa Word a -> [(code, Maybe a, a)] -> Pa Word a
    toSymbSet acc ((_, Just a, b):ts)
      = toSymbSet (Cons 1 b $ Cons 0 a acc) ts
    toSymbSet acc ((_, _, b):ts) -- Only for the first interval.
      = toSymbSet (Cons 1 b acc) ts
    toSymbSet acc@(Cons _ s _) []
      -- Add maxBound to the CharSet.
      | s /= maxBound = reversePa acc (Cons 0 maxBound Nil)
      -- Only one symbol set has maxBound inside.
      | otherwise     = reversePa acc Nil
    toSymbSet _ _ = moduleError "toSymbSet" "no intervals"

toPartition :: SymbSet a -> Pa Word a
toPartition (S pa) = pa

member :: Ord a => a -> SymbSet a -> Bool
member s (S pa) = isInside pa
  where
    isInside (Cons code symb ps)
      -- Character belongs to the current interval.
      | s <= symb = code == 1
      | otherwise = isInside ps
    isInside _ = moduleError "isInside" "invalid set"

firstSymb :: (Eq a, Bounded a, Enum a) => SymbSet a -> a
firstSymb (S (Cons code symb _))
  | code /= 0        = minBound
  | symb /= maxBound = succ symb
  | otherwise        = moduleError "firstSymb" "empty set"
firstSymb _ = moduleError "firstSymb" "no intervals"

complement :: SymbSet a -> SymbSet a
complement (S pa) = S $ compl pa
  where
    compl (Cons 0 symb ps) = Cons 1 symb $ compl ps
    compl (Cons _ symb ps) = Cons 0 symb $ compl ps    
    compl Nil              = Nil

union :: Ord a => SymbSet a -> SymbSet a -> SymbSet a
union (S as) (S bs) = S $ merge (.|.) as bs

intersect :: Ord a => SymbSet a -> SymbSet a -> SymbSet a
intersect (S as) (S bs) = S $ merge (.&.) as bs

merge :: Ord a => (Word -> Word -> Word) -> Pa Word a -> Pa Word a -> Pa Word a
merge op xss@(Cons code _  _) yss@(Cons code' _ _)
  = merge' (code `op` code') undefined {- prevSymb can be any value -}
           Nil xss yss
  where
    merge' prevCode prevSymb acc pss@(Cons c s ps) pss'@(Cons c' s' ps')
      = case compare s s' of
          LT -> cont s  ps  pss'
          GT -> cont s' pss ps'
          EQ -> cont s  ps  ps'
      where
        newCode = c `op` c'
        cont symb
          | newCode == prevCode = merge' prevCode symb acc
          | otherwise           = merge' newCode symb
                                         (Cons prevCode prevSymb acc)
    merge' prevCode prevSymb acc _ _
      = reversePa acc (Cons prevCode prevSymb Nil)
merge _ _ _ = moduleError "merge" "no intervals"

instance Show (Range Char) where
  showsPrec _ (R a b)
    | a == b    = (++) (esc a)
    | otherwise = (++) (esc a ++ '-':esc b)
    where
      esc c | c `elem` "]-^# " = '\\':c:""
      esc c = escapeSpecial c

instance Show (SymbSet Char) where
  showsPrec _ pa
    = ('[':) . foldl (.) id xs . (']':)
    where
      xs :: [String -> String]
      xs = intersperse (' ':) $ map shows $ toRanges pa
