-- |
-- Module    : Core.RE
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Simplified regular expressions.
--
module Core.RE
  ( RE
  , toRE
  , fromRE
  , nullable
  , derivative
  , partitionAlphabetByDerivatives
  , partitionAlphabetByDerivativesMany
  ) where

import Data.List (sort)
import qualified Core.Regex as R
import Core.SymbSet
import Core.Utils
import Core.Partition
import Data.Monoid

-- |Type @'RE'@ represents regular expressions. @RE@ is lighter version
-- of @Regex@ which is designed for use in automata constructions. Therefore
-- it does not have counters, capturing groups and flags for laziness.
data RE = RCharSet CharSet
        | REpsilon
        | ROr [RE]
        | RAnd [RE]
        | RConcat [RE]
        | RStar RE
        | RNot RE
        deriving Ord

-- This is slightly faster than code generated by GHC.
instance Eq RE where
  RCharSet as == RCharSet bs = as == bs
  REpsilon    == REpsilon    = True
  ROr as      == ROr bs      = as == bs
  RAnd as     == RAnd bs     = as == bs
  RConcat as  == RConcat bs  = as == bs
  RStar a     == RStar b     = a  == b
  RNot a      == RNot b      = a  == b
  _           == _           = False

{-
-- Code generated by GHC is much faster.
instance Ord RE where
  RCharSet as <= RCharSet bs = as <= bs
  RCharSet _  <= _           = True
  REpsilon    <= RCharSet _  = False
  REpsilon    <= _           = True
  ROr _       <= RCharSet _  = False
  ROr _       <= REpsilon    = False
  ROr as      <= ROr bs      = as <= bs
  ROr _       <= _           = True
  RAnd _      <= RCharSet _  = False
  RAnd _      <= REpsilon    = False
  RAnd _      <= ROr _       = False
  RAnd as     <= RAnd bs     = as <= bs
  RAnd _      <= _           = True
  RConcat _   <= RCharSet _  = False
  RConcat _   <= REpsilon    = False
  RConcat _   <= ROr _       = False
  RConcat _   <= RAnd _      = False
  RConcat as  <= RConcat bs  = as <= bs
  RConcat _   <= _           = True
  RStar _     <= RCharSet _  = False
  RStar _     <= REpsilon    = False
  RStar _     <= ROr _       = False
  RStar _     <= RAnd _      = False
  RStar _     <= RConcat _   = False
  RStar a     <= RStar b     = a <= b
  RStar _     <= _           = True
  RNot a      <= RNot b      = a <= b
  RNot _      <= _           = False
-}

instance Show RE where
  showsPrec p r = showsPrec p (fromRE r)

-- |Converts @RE@ to @Regex@.
fromRE :: RE -> R.Regex
fromRE (RCharSet cs) = R.RCharSet cs
fromRE REpsilon      = R.REpsilon
fromRE (ROr rs)      = R.ROr (map fromRE rs)
fromRE (RAnd rs)     = R.RAnd (map fromRE rs)
fromRE (RConcat rs)  = R.RConcat (map fromRE rs)
fromRE (RStar r)     = R.RCounter R.Greedy 0 Nothing (fromRE r)
fromRE (RNot r)      = R.RNot (fromRE r)

-- |Converts @Regex@ to @RE@.
--
-- Counters are rewritten with @concatenation@, operator @or@ and operator
-- @star@:
--
-- * @x{n,}@ is rewritten into @x^n x*@.
--
-- * @x{n,m}@ is rewritten into @x^n (? x | )^(m-n)@.
toRE :: R.Regex -> RE
toRE R.REpsilon      = REpsilon
toRE (R.RCharSet cs) = RCharSet cs
toRE (R.ROr rs)      = simpOr (map toRE rs)
toRE (R.RAnd rs)     = simpAnd (map toRE rs)
toRE (R.RConcat rs)  = simpConcat (map toRE rs)
toRE (R.RCounter _ minRep maxRep' r)
  = case maxRep' of
      Nothing     -> simpConcat $ mandatory ++ [simpStar newR]
      Just maxRep -> simpConcat $ mandatory ++ (replicate (maxRep-minRep)
                                                  (simpOr [newR, REpsilon]))
  where
    mandatory = replicate minRep newR  -- Minimal number of repetitions.
    newR      = toRE r
toRE (R.RNot r)      = simpNot (toRE r)
toRE (R.RGroup _ r)  = toRE r  -- Capturing groups are not supported.

-- |Applies operator @or@ to the list of simplified regular expressions.
-- Returns simplified regular expression.
simpOr :: [RE] -> RE
simpOr = fin . nubSorted . sort . unionCharSets . concatMap item
  where
    item (ROr xs)         = xs  -- Take up nested disjunction.
    item x | x == minLang = []  -- Ignore empty language.
           | otherwise    = [x]
    unionCharSets = merge . separateCharSets
      where
        -- @cs@ is a list of @CharSet@s from disjunction which will be merged
        -- and @os@ are other regular expressions which were in disjunction.
        merge ([], os) = os
        merge (cs, os) = (RCharSet $ foldl union empty cs):os
    fin []                     = minLang  -- Empty disjunction.
    fin [x]                    = x
    fin xs | maxLang `elem` xs = maxLang  -- Language with all words.
           | otherwise         = ROr xs

-- |Applies operator @and@ to the list of simplified regular expressions.
-- Returns simplified regular expression.
simpAnd :: [RE] -> RE
simpAnd = fin . nubSorted . sort . intersectCharSets . concatMap item
  where
    item (RAnd xs)        = xs  -- Take up nested conjunction.
    item x | x == maxLang = []  -- Ignore language with all words.
           | otherwise    = [x]
    intersectCharSets = merge . separateCharSets
      where
        -- @cs@ is a list of @CharSet@s from conjunction.
        merge ([], os) = os
        merge (cs, os) = (RCharSet $ foldl intersect alphabet cs):os
    fin []                      = maxLang  -- Empty conjunction.
    fin [x]                     = x
    fin xs | head xs == minLang = minLang  -- Empty language.
           | otherwise          = RAnd xs

-- |Concatenates simplified regular expressions in the given list. Returns
-- simplified regular expression.
simpConcat :: [RE] -> RE
simpConcat = fin . concatMap item
  where
    item (RConcat xs) = xs  -- Take up nested concatenation.
    item REpsilon     = []  -- Ignore empty string.
    item x            = [x]
    fin []  = REpsilon  -- Empty concatenation.
    fin [x] = x
    fin xs | minLang `elem` xs = minLang  -- Empty language.
           | otherwise         = RConcat xs

-- |Applies operator @star@ to simplified regular expression. Returns
-- simplified regular expression.
simpStar :: RE -> RE
simpStar r = case r of
               REpsilon           -> REpsilon  -- Empty string.
               RStar _            -> r         -- Idempotence.
               ROr (x@(RCharSet cs):_)
                 -- Disjunction with whole alphabet.
                 | cs == alphabet -> RStar x
               _ | r == minLang   -> REpsilon  -- Empty language.
                 | otherwise      -> RStar r

-- |Applies operator @not@ to simplified regular expression. Returns
-- simplified regular expression.
simpNot :: RE -> RE
simpNot r = case r of
              RNot r' -> r'  -- Double not.
              _ | r == minLang -> maxLang  -- Empty language.
                | r == maxLang -> minLang  -- Language with all words.
                | otherwise    -> RNot r

-- |Function @'separateCharSets' rs@ returns a pair @(cs, os)@ where @cs@
-- contains character sets from @rs@ (i.e. if @RCharSet xs `elem` rs@ then
-- @xs `elem` cs@). @os@ contains regular expression from @rs@ which doesn't
-- pattern match with @RCharSet _@.
separateCharSets :: [RE] -> ([CharSet], [RE])
separateCharSets = p [] []
  where
    p cs os (x:xs) = case x of RCharSet x' -> p (x':cs) os xs
                               _           -> p cs (x:os) xs
    p cs os _      = (cs, os)

-- |Empty language.
minLang :: RE
minLang = RCharSet empty

-- |Language with all words.
maxLang :: RE
maxLang = RStar $ RCharSet alphabet

-- |Returns @True@ when the language of the given regular expression contains
-- empty string.
nullable :: RE -> Bool
nullable (RCharSet _) = False
nullable REpsilon     = True
nullable (ROr rs)     = or (map nullable rs)
nullable (RAnd rs)    = and (map nullable rs)
nullable (RConcat rs) = and (map nullable rs)
nullable (RStar _)    = True
nullable (RNot r)     = not (nullable r)

-- |The function @'derivative' c r@ returns simplified left derivative
-- of simplified regular expression @r@ by character @c@.
derivative :: Char -> RE -> RE
derivative c (RCharSet cs)  = if member c cs then REpsilon else RCharSet empty
derivative _ REpsilon       = RCharSet empty
derivative c (ROr rs)       = simpOr $ map (derivative c) rs
derivative c (RAnd rs)      = simpAnd $ map (derivative c) rs
derivative _ (RConcat [])   = RCharSet empty
derivative c (RConcat (r:rs))
  | nullable r = simpOr [first, derivative c rest]
  | otherwise  = first
  where
    first = simpConcat [derivative c r, rest]
    rest  = simpConcat rs
derivative c star@(RStar r) = simpConcat [derivative c r, star]
derivative c (RNot r)       = simpNot (derivative c r)

-- |The function (@'partitionAlphabetByDerivatives' r@) returns a partition
-- of the alphabet where each block of the partition contains characters
-- which give same derivatives for @r@ i.e. if @c1@ and @c2@ are in the same
-- block then @'derivative' c1 r == 'derivative' c2 r@.
partitionAlphabetByDerivatives :: RE -> Pa Char
partitionAlphabetByDerivatives re
  = case re of
      RCharSet cs    -> toPartition cs
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

-- |Similar to @'partitionAlphabetByDerivatives'@ but accepts list of regular
-- expressions.
partitionAlphabetByDerivativesMany :: [RE] -> Pa Char
partitionAlphabetByDerivativesMany rs
  = mconcat (map partitionAlphabetByDerivatives rs)
{-# INLINE partitionAlphabetByDerivativesMany #-}

