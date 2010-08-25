{-# LANGUAGE GADTs,
             EmptyDataDecls #-}

-- |
-- Module    : Core.Regex
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Regular expressions.
--
module Core.Regex
       (
         Regex(..)
       , Yes
       , No
       , reverseRegex
       , removeCaptures
       , toRegexWithCaptures
       , listCaptures
       ) where

import Core.Set

-- | Type @'Regex' p s c@ represents regular expressions where
--
-- * @s@ is type of symbols,
--
-- * @p s@ represents set of symbols,
--
-- * flag @c@ affects whether constructor 'Capture' can be used.
data Regex s c where
  -- @'Epsilon'@ denotes @L = {empty word}@.
  Epsilon :: Regex s c
  -- @'CharClass' set@ denotes @L = set@.
  CharClass :: Set s -> Regex s c
  -- @'Or' a b@ denotes @L = L(a) \\\/ L(b)@.
  Or :: Regex s c -> Regex s c -> Regex s c
  -- @'And' a b@ denotes @L = L(a) \/\\ L(b)@.
  And :: Regex s c -> Regex s c -> Regex s c
  -- @'Concat' a b@ denotes @L = {uv | u in L(a), v in L(b)}@.
  Concat :: Regex s c -> Regex s c -> Regex s c
  -- @'RepeatU' lo a@ denotes
  -- @L = {u_1 u_2 ... u_n | u_i in L(a), n is natural number, lo <= n}@.
  RepeatU :: !Int -> Regex s c -> Regex s c
  -- @'Repeat' lo hi a@ denotes
  -- @L = {u_1 ... u_n | u_i in L(a), n is natural number, lo <= n <= hi}@.
  Repeat :: !Int -> !Int -> Regex s c -> Regex s c
  -- @'Not' a@ denotes @L = all words except those in L(a)@.
  Not :: Regex s No -> Regex s c
  -- @'Capture' i a@ denotes @L = L(a)@.
  Capture :: !Int -> Regex s c -> Regex s Yes

-- | Phantom for 'Regex'.
data Yes

-- | Phantom for 'Regex'.
data No

-- | Reversed regular expression matches reversed words.
reverseRegex :: Regex s c -> Regex s c
reverseRegex Epsilon = Epsilon
reverseRegex r@(CharClass _) = r
reverseRegex (Or a b) = reverseRegex a `Or` reverseRegex b
reverseRegex (And a b) = reverseRegex a `And` reverseRegex b
reverseRegex (Concat a b) = reverseRegex b `Concat` reverseRegex a
reverseRegex (RepeatU lo a) = RepeatU lo (reverseRegex a)
reverseRegex (Repeat lo hi a) = Repeat lo hi (reverseRegex a)
reverseRegex (Not a) = Not (reverseRegex a)
reverseRegex (Capture i a) = Capture i (reverseRegex a)

-- | Subexpressions matching @'Capture' _ r@ are replaced by @r@.
removeCaptures :: Regex s c -> Regex s No
removeCaptures Epsilon          = Epsilon
removeCaptures (CharClass set)  = CharClass set
removeCaptures (Or a b)         = removeCaptures a `Or` removeCaptures b
removeCaptures (And a b)        = removeCaptures a `And` removeCaptures b
removeCaptures (Concat a b)     = removeCaptures a `Concat` removeCaptures b
removeCaptures (RepeatU lo a)   = RepeatU lo $ removeCaptures a
removeCaptures (Repeat lo hi a) = Repeat lo hi $ removeCaptures a
removeCaptures (Not a)          = Not a
removeCaptures (Capture _ a)    = removeCaptures a

-- | Only for changing type signature.
toRegexWithCaptures :: Regex s c -> Regex s Yes
toRegexWithCaptures Epsilon          = Epsilon
toRegexWithCaptures (CharClass set)  = CharClass set
toRegexWithCaptures (Or a b)         = toRegexWithCaptures a `Or`
                                       toRegexWithCaptures b
toRegexWithCaptures (And a b)        = toRegexWithCaptures a `And`
                                       toRegexWithCaptures b
toRegexWithCaptures (Concat a b)     = toRegexWithCaptures a `Concat`
                                       toRegexWithCaptures b
toRegexWithCaptures (RepeatU lo r)   = RepeatU lo $ toRegexWithCaptures r
toRegexWithCaptures (Repeat lo hi r) = Repeat lo hi $ toRegexWithCaptures r
toRegexWithCaptures (Not r)          = Not r
toRegexWithCaptures r@(Capture _ _)  = r

-- | List of all @i@s from subexpressions matching @'Capture' i _@.
listCaptures :: Regex s c -> [Int]
listCaptures Epsilon        = []
listCaptures (CharClass _)  = []
listCaptures (Or a b)       = listCaptures a ++ listCaptures b
listCaptures (And a b)      = listCaptures a ++ listCaptures b
listCaptures (Concat a b)   = listCaptures a ++ listCaptures b
listCaptures (RepeatU _ a)  = listCaptures a
listCaptures (Repeat _ _ a) = listCaptures a
listCaptures (Not _)        = []
listCaptures (Capture i a)  = i:listCaptures a
