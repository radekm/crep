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
data RE p s = RCharClass (p s)
            | REpsilon
            | ROr [RE p s]
            | RAnd [RE p s]
            | RConcat [RE p s]
            | RStar (RE p s)
            | RNot (RE p s)
            deriving (Eq, Ord)

-- | Converts @Regex@ to @RE@.
toRE :: (Pa p s, Ord (p s)) => Regex p s c -> RE p s
toRE Epsilon         = REpsilon
toRE (CharClass set) = RCharClass set
toRE (Or a b)        = consOr [toRE a, toRE b]
toRE (And a b)       = consAnd [toRE a, toRE b]
toRE (Concat a b)    = consConcat [toRE a, toRE b]
toRE (Star a)        = consStar $ toRE a
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
consOr :: (Pa p s, Ord (p s)) => [RE p s] -> RE p s
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
consAnd :: (Pa p s, Ord (p s)) => [RE p s] -> RE p s
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
consConcat :: (Eq (p s), Pa p s) => [RE p s] -> RE p s
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
consStar :: (Eq (p s), Pa p s) => RE p s -> RE p s
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
consNot :: (Eq (p s), Pa p s) => RE p s -> RE p s
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
separateSymbolSets :: [RE p s] -> ([p s], [RE p s])
separateSymbolSets = p [] []
  where
    p sets os (r:rs) = case r of RCharClass set -> p (set:sets) os     rs
                                 _              -> p sets       (r:os) rs
    p sets os []     = (sets, os)

-- | Empty language.
minLang :: Pa p s => RE p s
minLang = RCharClass empty

-- | Language with all words.
maxLang :: Pa p s => RE p s
maxLang = RStar $ RCharClass alphabet

-- | Returns whether given regular expression matches empty word.
nullable :: RE p s -> Bool
nullable (RCharClass _) = False
nullable REpsilon       = True
nullable (ROr rs)       = or (map nullable rs)
nullable (RAnd rs)      = and (map nullable rs)
nullable (RConcat rs)   = and (map nullable rs)
nullable (RStar _)      = True
nullable (RNot r)       = not (nullable r)

-- | The function @'derivative' c r@ returns left derivative
--   of regular expression @r@ by character @c@.
derivative :: (Pa p s, Ord (p s)) => s -> RE p s -> RE p s
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
partitionAlphabetByDerivatives :: Monoid (p s) => RE p s -> p s
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
partitionAlphabetByDerivativesMany :: Monoid (p s) => [RE p s] -> p s
partitionAlphabetByDerivativesMany rs
  = mconcat (map partitionAlphabetByDerivatives rs)
{-# INLINE partitionAlphabetByDerivativesMany #-}
