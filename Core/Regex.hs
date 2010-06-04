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
  Star :: Regex p s c -> Regex p s c
  -- @'Repeat' lo hi a@ denotes
  -- @L = {u_1 ... u_n | u_i in L(a), n is natural number, lo <= n <= hi}@.
  Repeat :: !Int -> !Int -> Regex p s c -> Regex p s c
  -- @'Not' a@ denotes @L = all words except those in L(a)@.
  Not :: Regex p s No -> Regex p s c
  -- @'Capture' i a@ denotes @L = L(a)@.
  Capture :: !Int -> Regex p s c -> Regex p s Yes

-- | Phantom for 'Regex'.
data Yes

-- | Phantom for 'Regex'.
data No
