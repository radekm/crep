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

-- | Type @'Regex' p s r c@ represents regular expressions where
--
-- * @s@ is type of symbols,
--
-- * @p s@ represents set of symbols,
--
-- * flag @r@ affects whether constructor 'Repeat' can be used,
--
-- * flag @c@ affects whether constructor 'Capture' can be used.
data Regex p s r c where
  -- @'Epsilon'@ denotes @L = {empty word}@.
  Epsilon :: Regex p s r c
  -- @'CharClass' set@ denotes @L = set@.
  CharClass :: p s -> Regex p s r c
  -- @'Or' a b@ denotes @L = L(a) \\\/ L(b)@.
  Or :: Regex p s r c -> Regex p s r c -> Regex p s r c
  -- @'And' a b@ denotes @L = L(a) \/\\ L(b)@.
  And :: Regex p s r c -> Regex p s r c -> Regex p s r c
  -- @'Concat' a b@ denotes @L = {uv | u in L(a), v in L(b)}@.
  Concat :: Regex p s r c -> Regex p s r c -> Regex p s r c
  -- @'Star' a@ denotes
  -- @L = {u_1 u_2 ... u_n | u_i in L(a), n is natural number}@.
  Star :: Regex p s r c -> Regex p s r c
  -- @'Repeat' lo hi a@ denotes
  -- @L = {u_1 ... u_n | u_i in L(a), n is natural number, lo <= n <= hi}@.
  Repeat :: !Int -> !Int -> Regex p s r c -> Regex p s Yes c
  -- @'Not' a@ denotes @L = all words except those in L(a)@.
  Not :: Regex p s r No -> Regex p s r c
  -- @'Capture' i a@ denotes @L = L(a)@.
  Capture :: !Int -> Regex p s r c -> Regex p s r Yes

-- | Phantom for 'Regex'.
data Yes

-- | Phantom for 'Regex'.
data No
