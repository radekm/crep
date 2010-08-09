{-# LANGUAGE GADTs #-}

-- |
-- Module    : Core.RE
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- This module contains type 'RE' which represents regular expressions
-- in normal form.
--
module Core.RE
       (
         RE
       , toRE
       , nullable
       , derivative
       , partitionAlphabetByDerivatives
       , partitionAlphabetByDerivativesMany
       ) where

import Data.List (sort)
import Core.Regex
import Core.Utils
import Core.Partition
import Data.Monoid

-- | Type @'RE' p s@ represents regular expressions in normal form where
--
-- * @s@ is type of symbols,
--
-- * @p s@ represents set of symbols.
data RE s = RCharClass (Pa s)
          | REpsilon
          | ROr [RE s]
          | RAnd [RE s]
          | RConcat [RE s]
          | RStar (RE s)
          | RNot (RE s)
          deriving (Eq, Ord)

-- | Converts @Regex@ to @RE@.
toRE :: Symbol s => Regex s c -> RE s
toRE Epsilon         = REpsilon
toRE (CharClass set) = RCharClass set
toRE (Or a b)        = consOr [toRE a, toRE b]
toRE (And a b)       = consAnd [toRE a, toRE b]
toRE (Concat a b)    = consConcat [toRE a, toRE b]
toRE (RepeatU lo a)  = consConcat $ mandatory ++ [consStar newA]
  where
    mandatory = replicate lo newA
    newA      = toRE a
toRE (Repeat lo hi a)
  = consConcat $ mandatory ++ (replicate (hi-lo)
                                         (consOr [newA, REpsilon]))
  where
    mandatory = replicate lo newA  -- Minimal number of repetitions.
    newA      = toRE a
toRE (Not a)         = consNot (toRE a)
toRE (Capture _ a)   = toRE a

-- | Applies 'ROr' to the list of normalized regular expressions.
--   Returns normalized regular expression.
consOr :: Symbol s => [RE s] -> RE s
consOr = fin . nubSorted . sort . unionCharClasses . concatMap item
  where
    item (ROr xs)         = xs  -- Take up nested disjunction.
    item x | x == minLang = []  -- Ignore empty language.
           | otherwise    = [x]
    unionCharClasses = merge . separateSymbolSets
      where
        merge ([],   os) = os
        merge (sets, os) = (RCharClass $ foldl union empty sets):os
    fin []                     = minLang  -- Empty disjunction.
    fin [x]                    = x
    fin xs | maxLang `elem` xs = maxLang  -- Language with all words.
           | otherwise         = ROr xs

-- | Applies 'RAnd' to the list of normalized regular expressions.
--   Returns normalized regular expression.
consAnd :: Symbol s => [RE s] -> RE s
consAnd = fin . nubSorted . sort . intersectCharClasses . concatMap item
  where
    item (RAnd xs)        = xs  -- Take up nested conjunction.
    item x | x == maxLang = []  -- Ignore language with all words.
           | otherwise    = [x]
    intersectCharClasses = merge . separateSymbolSets
      where
        merge ([],   os) = os
        merge (sets, os) = (RCharClass $ foldl intersect alphabet sets):os
    fin []                      = maxLang  -- Empty conjunction.
    fin [x]                     = x
    fin xs | head xs == minLang = minLang  -- Empty language.
           | otherwise          = RAnd xs

-- | Applies 'RConcat' to the list of normalized regular expressions.
--   Returns normalized regular expression.
consConcat :: Symbol s => [RE s] -> RE s
consConcat = fin . concatMap item
  where
    item (RConcat xs) = xs  -- Take up nested concatenation.
    item REpsilon     = []  -- Ignore empty string.
    item x            = [x]
    fin []  = REpsilon  -- Empty concatenation.
    fin [x] = x
    fin xs | minLang `elem` xs = minLang  -- Empty language.
           | otherwise         = RConcat xs

-- | Applies 'RStar' to the normalized regular expression.
--   Returns normalized regular expression.
consStar :: Symbol s => RE s -> RE s
consStar r = case r of
               REpsilon           -> REpsilon  -- Empty string.
               RStar _            -> r         -- Idempotence.
               ROr (x@(RCharClass set):_)
                 -- Disjunction with whole alphabet.
                 | set == alphabet -> RStar x
               _ | r == minLang    -> REpsilon  -- Empty language.
                 | otherwise       -> RStar r

-- | Applies 'RStar' to the normalized regular expression.
--   Returns normalized regular expression.
consNot :: Symbol s => RE s -> RE s
consNot r = case r of
              RNot r' -> r'  -- Double not.
              _ | r == minLang -> maxLang  -- Empty language.
                | r == maxLang -> minLang  -- Language with all words.
                | otherwise    -> RNot r

-- | Function @'separateSymbolSets' rs@ returns pair @(sets, os)@ where
--
-- * @sets@ is a list of symbol sets from regular expressions in @rs@
--   matching @'RCharClass' set@,
--
-- * @os@ is a list of regular expressions which don't match @'RCharClass' _@.
separateSymbolSets :: [RE s] -> ([Pa s], [RE s])
separateSymbolSets = p [] []
  where
    p sets os (r:rs) = case r of RCharClass set -> p (set:sets) os     rs
                                 _              -> p sets       (r:os) rs
    p sets os []     = (sets, os)

-- | Empty language.
minLang :: Symbol s => RE s
minLang = RCharClass empty

-- | Language with all words.
maxLang :: Symbol s => RE s
maxLang = RStar $ RCharClass alphabet

-- | Returns whether given regular expression matches empty word.
nullable :: RE s -> Bool
nullable (RCharClass _) = False
nullable REpsilon       = True
nullable (ROr rs)       = or (map nullable rs)
nullable (RAnd rs)      = and (map nullable rs)
nullable (RConcat rs)   = and (map nullable rs)
nullable (RStar _)      = True
nullable (RNot r)       = not (nullable r)

-- | The function @'derivative' c r@ returns left derivative
--   of regular expression @r@ by character @c@.
derivative :: Symbol s => s -> RE s -> RE s
derivative c (RCharClass set)
  | member c set = REpsilon
  | otherwise    = RCharClass empty
derivative _ REpsilon        = RCharClass empty
derivative c (ROr rs)        = consOr $ map (derivative c) rs
derivative c (RAnd rs)       = consAnd $ map (derivative c) rs
derivative _ (RConcat [])    = RCharClass empty
derivative c (RConcat (r:rs))
  | nullable r = consOr [first, derivative c rest]
  | otherwise  = first
  where
    first = consConcat [derivative c r, rest]
    rest  = consConcat rs
derivative c star@(RStar r) = consConcat [derivative c r, star]
derivative c (RNot r)       = consNot (derivative c r)

-- | Function @'partitionAlphabetByDerivatives' r@ returns partition
--   of the alphabet where derivatives of @r@ by characters from the same
--   block are same.
partitionAlphabetByDerivatives :: Symbol s => RE s -> Pa s
partitionAlphabetByDerivatives re
  = case re of
      RCharClass set -> set
      REpsilon       -> mempty
      ROr rs         -> pam rs
      RAnd rs        -> pam rs
      RConcat []     -> mempty
      RConcat (r:rs)
        | nullable r -> mappend (pa r) (pa $ RConcat rs)
        | otherwise  -> pa r
      RStar r        -> pa r
      RNot r         -> pa r
  where
    pa  = partitionAlphabetByDerivatives
    pam = partitionAlphabetByDerivativesMany

-- | Function @'partitionAlphabetByDerivativesMany' rs@ returns partition
--   of the alphabet where derivatives of @rs@ by characters from the same
--   block are same.
--
--   Note: Derivative of list is list of derivatives.
partitionAlphabetByDerivativesMany :: Symbol s => [RE s] -> Pa s
partitionAlphabetByDerivativesMany rs
  = mconcat (map partitionAlphabetByDerivatives rs)
{-# INLINE partitionAlphabetByDerivativesMany #-}
