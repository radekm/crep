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
       , removeCaptures
       , listCaptures
       ) where

-- | Type @'Regex' p s c@ represents regular expressions where
--
-- * @s@ is type of symbols,
--
-- * @p s@ represents set of symbols,
--
-- * flag @c@ affects whether constructor 'Capture' can be used.
data Regex p s c where
  -- @'Epsilon'@ denotes @L = {empty word}@.
  Epsilon :: Regex p s c
  -- @'CharClass' set@ denotes @L = set@.
  CharClass :: p s -> Regex p s c
  -- @'Or' a b@ denotes @L = L(a) \\\/ L(b)@.
  Or :: Regex p s c -> Regex p s c -> Regex p s c
  -- @'And' a b@ denotes @L = L(a) \/\\ L(b)@.
  And :: Regex p s c -> Regex p s c -> Regex p s c
  -- @'Concat' a b@ denotes @L = {uv | u in L(a), v in L(b)}@.
  Concat :: Regex p s c -> Regex p s c -> Regex p s c
  -- @'Star' a@ denotes
  -- @L = {u_1 u_2 ... u_n | u_i in L(a), n is natural number}@.
  Star :: Regex p s No -> Regex p s c
  -- @'Repeat' lo hi a@ denotes
  -- @L = {u_1 ... u_n | u_i in L(a), n is natural number, lo <= n <= hi}@.
  Repeat :: !Int -> !Int -> Regex p s No -> Regex p s c
  -- @'Not' a@ denotes @L = all words except those in L(a)@.
  Not :: Regex p s No -> Regex p s c
  -- @'Capture' i a@ denotes @L = L(a)@.
  Capture :: !Int -> Regex p s c -> Regex p s Yes

-- | Phantom for 'Regex'.
data Yes

-- | Phantom for 'Regex'.
data No

-- | Subexpressions matching @'Capture' _ r@ are replaced by @r@.
removeCaptures :: Regex p s c -> Regex p s No
removeCaptures Epsilon          = Epsilon
removeCaptures (CharClass set)  = CharClass set
removeCaptures (Or a b)         = removeCaptures a `Or` removeCaptures b
removeCaptures (And a b)        = removeCaptures a `And` removeCaptures b
removeCaptures (Concat a b)     = removeCaptures a `Concat` removeCaptures b
removeCaptures (Star a)         = Star a
removeCaptures (Repeat lo hi a) = Repeat lo hi a
removeCaptures (Not a)          = Not a
removeCaptures (Capture _ a)    = removeCaptures a

-- | List of all @i@s from subexpressions matching @'Capture' i _@.
listCaptures :: Regex p s c -> [Int]
listCaptures Epsilon        = []
listCaptures (CharClass _)  = []
listCaptures (Or a b)       = listCaptures a ++ listCaptures b
listCaptures (And a b)      = listCaptures a ++ listCaptures b
listCaptures (Concat a b)   = listCaptures a ++ listCaptures b
listCaptures (Star _)       = []
listCaptures (Repeat _ _ _) = []
listCaptures (Not _)        = []
listCaptures (Capture i a)  = i:listCaptures a
