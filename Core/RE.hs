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

import Core.Regex
import Core.Partition
import Data.Monoid

-- | Type @'RE' s@ represents regular expressions in normal form where
--   @s@ is type of symbols.
data RE s = RCharClass (Pa s)
          | REpsilon
          | ROr (RE s) (RE s)
          | RAnd (RE s) (RE s)
          | RConcat (RE s) (RE s)
          | RStar (RE s)
          | RNot (RE s)
          deriving (Eq, Ord)

-- | Converts @Regex@ to @RE@.
toRE :: Symbol s => Regex s c -> RE s
toRE Epsilon         = REpsilon
toRE (CharClass set) = RCharClass set
toRE (Or a b)        = consOr (toRE a) (toRE b)
toRE (And a b)       = consAnd (toRE a) (toRE b)
toRE (Concat a b)    = consConcat (toRE a) (toRE b)
toRE (RepeatU lo a)
  | lo == 0 = consStar newA
  | lo > 0 = consConcat newA (toRE $ RepeatU (lo - 1) a)
  | otherwise = error "tore - repeatu"
  where
    newA      = toRE a
toRE (Repeat lo hi a)
  | lo == 0 && hi == 0 = REpsilon
  | lo == 0 && hi > 0 = consConcat (consOr newA REpsilon)
                                   (toRE $ Repeat lo (hi - 1) a)
  | lo > 0 && hi > 0 = consConcat newA (toRE $ Repeat (lo - 1) (hi - 1) a)
  | otherwise = error "tore - repeat"
  where
    newA      = toRE a
toRE (Not a)         = consNot (toRE a)
toRE (Capture _ a)   = toRE a

-- | Disjunction of two normalized regular expressions.
--
--   Associativity, commutativity, min lang, max lang, duplicate removal.
consOr :: Symbol s => RE s -> RE s -> RE s
consOr x y
  | maxLang `elem` [x, y] = maxLang
  | minLang == x = y
  | minLang == y = x
  | otherwise    = unionCharClasses $ mergeOr x y
  where
    unionCharClasses (ROr (RCharClass c1) (ROr (RCharClass c2) rest))
      = ROr (RCharClass $ union c1 c2) rest
    unionCharClasses (ROr (RCharClass c1) (RCharClass c2))
      = RCharClass $ union c1 c2
    unionCharClasses r = r

    mergeOr aa@(ROr a a') bb@(ROr b b')
      = case compare a b of
          LT -> ROr a (mergeOr a' bb)
          GT -> ROr b (mergeOr aa b')
          EQ -> mergeOr a' bb
    mergeOr a bb@(ROr b b')
      = case compare a b of
          LT -> ROr a bb
          GT -> ROr b (mergeOr a b')
          EQ -> bb
    mergeOr aa@(ROr _ _) b = mergeOr b aa
    mergeOr a b
      = case compare a b of
          LT -> ROr a b
          GT -> ROr b a
          EQ -> a

-- | Conjunction of two normalized regular expressions.
--
--   Associativity, commutativity, min lang, max lang, duplicate removal.
consAnd :: Symbol s => RE s -> RE s -> RE s
consAnd x y
  | minLang `elem` [x, y] = minLang
  | maxLang == x = y
  | maxLang == y = x
  | otherwise    = intersectCharClasses $ mergeAnd x y
  where
    intersectCharClasses (RAnd (RCharClass c1) (RAnd (RCharClass c2) rest))
      | c == empty = minLang
      | otherwise  = RAnd (RCharClass $ intersect c1 c2) rest
      where
        c = intersect c1 c2
    intersectCharClasses (RAnd (RCharClass c1) (RCharClass c2))
      = RCharClass $ intersect c1 c2
    intersectCharClasses r = r

    mergeAnd aa@(RAnd a a') bb@(RAnd b b')
      = case compare a b of
          LT -> RAnd a (mergeAnd a' bb)
          GT -> RAnd b (mergeAnd aa b')
          EQ -> mergeAnd a' bb
    mergeAnd a bb@(RAnd b b')
      = case compare a b of
          LT -> RAnd a bb
          GT -> RAnd b (mergeAnd a b')
          EQ -> bb
    mergeAnd aa@(RAnd _ _) b = mergeAnd b aa
    mergeAnd a b
      = case compare a b of
          LT -> RAnd a b
          GT -> RAnd b a
          EQ -> a

-- | Concatenation of two normalized regular expressions.
--
--   Associativity, epsilon, min lang.
consConcat :: Symbol s => RE s -> RE s -> RE s
consConcat x y
  | minLang `elem` [x, y] = minLang
  | REpsilon == x = y
  | REpsilon == y = x
  | otherwise     = mergeConcat x y
  where
    mergeConcat (RConcat a a') b
      = RConcat a (mergeConcat a' b)
    mergeConcat a b = RConcat a b

-- | Kleene's closure of normalized regular expression.
--
--   Epsilon, double star, min lang, alphabet.
consStar :: Symbol s => RE s -> RE s
consStar r = case r of
               REpsilon -> REpsilon
               RStar _ -> r
               ROr x@(RCharClass c) _y
                 | c == alphabet -> RStar x
               _
                 | r == minLang -> REpsilon
                 | otherwise    -> RStar r

-- | Complement of normalized regular expression.
--
--   Double negation, min lang, max lang.
consNot :: Symbol s => RE s -> RE s
consNot r = case r of
              RNot r' -> r'
              _
                | r == minLang -> maxLang
                | r == maxLang -> minLang
                | otherwise    -> RNot r

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
nullable (ROr a b)      = nullable a || nullable b
nullable (RAnd a b)     = nullable a && nullable b
nullable (RConcat a b)  = nullable a && nullable b
nullable (RStar _)      = True
nullable (RNot r)       = not (nullable r)

-- | The function @'derivative' c r@ returns left derivative
--   of regular expression @r@ by character @c@.
derivative :: Symbol s => s -> RE s -> RE s
derivative c (RCharClass set)
  | member c set = REpsilon
  | otherwise    = minLang
derivative _ REpsilon       = minLang
derivative c (ROr a b)      = consOr (derivative c a) (derivative c b)
derivative c (RAnd a b)     = consAnd (derivative c a) (derivative c b)
derivative c (RConcat a b)
  | nullable a = consOr first (derivative c b)
  | otherwise  = first
  where
    first = consConcat (derivative c a) b
derivative c star@(RStar r) = consConcat (derivative c r) star
derivative c (RNot r)       = consNot (derivative c r)

-- | Function @'partitionAlphabetByDerivatives' r@ returns partition
--   of the alphabet where derivatives of @r@ by characters from the same
--   block are same.
partitionAlphabetByDerivatives :: Symbol s => RE s -> Pa s
partitionAlphabetByDerivatives re
  = case re of
      RCharClass set -> set
      REpsilon       -> mempty
      ROr a b        -> pa a `mappend` pa b
      RAnd a b       -> pa a `mappend` pa b
      RConcat a b
        | nullable a -> pa a `mappend` pa b
        | otherwise  -> pa a
      RStar r        -> pa r
      RNot r         -> pa r
  where
    pa  = partitionAlphabetByDerivatives

-- | Function @'partitionAlphabetByDerivativesMany' rs@ returns partition
--   of the alphabet where derivatives of @rs@ by characters from the same
--   block are same.
--
--   Note: Derivative of list is list of derivatives.
partitionAlphabetByDerivativesMany :: Symbol s => [RE s] -> Pa s
partitionAlphabetByDerivativesMany rs
  = mconcat (map partitionAlphabetByDerivatives rs)
{-# INLINE partitionAlphabetByDerivativesMany #-}
