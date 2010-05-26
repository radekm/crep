{-# LANGUAGE GADTs,
             EmptyDataDecls #-}

-- |
-- Module    : Core.Regex
-- Copyright : (c) Radek Micek 2009-2010
-- License   : BSD3
-- Stability : experimental
--
-- Extended regular expressions.
--
module Core.Regex
       (
         Regex(..)
       , Yes
       , No
       , wrap
       ) where

import Core.SymbSet

-- |(@'Regex' a@) is type of extended regular expressions. Expressions
-- of type (@Regex Yes@) may contain subexpression @RCapture i r@, but
-- expressions of type (@Regex No@) must not. This restriction is due
-- to complement. It is not possible to capture what has matched
-- by subexpression of the complement because it has not matched.
data Regex group where
  -- |Empty word.
  Epsilon :: Regex a
  -- |Any character from given set.
  CharSet :: CharSet -> Regex a
  Or      :: Regex a -> Regex a -> Regex a
  And     :: Regex a -> Regex a -> Regex a
  Concat  :: Regex a -> Regex a -> Regex a
  -- |Repetition. (@RCounter min max r@) means, that expression @r@
  -- has to match at least @min@ and at most (@fromJust max@) times.
  -- (@max == Nothing@) means that there is no upper bound.
  Counter :: !Int -> !(Maybe Int) -> Regex a -> Regex a
  Not     :: Regex No -> Regex a
  -- |(@RCapture i r@) saves word matched by expression @r@ as @i@.
  Capture :: !Int -> Regex a -> Regex Yes

data Yes
data No

-- |Function (@wrap emptyRegex binOp rs@) converts list of regular expressions
-- @rs@ into one regular expression.
--
-- * Operation @binOp@ is left-associative.
--
-- * Returns @emptyRegex@ when the list is empty.
wrap :: Regex a -> (Regex a -> Regex a -> Regex a) -> [Regex a] -> Regex a
wrap emptyRegex _     [] = emptyRegex
wrap _          binOp xs = foldl1 binOp xs

-- |Type is used only when converting regular expression to string.
data RegexType
  = TEpsilon | TAtom | TNot | TCounter | TConcat | TAnd | TOr
  deriving (Eq, Ord)

instance Show (Regex a) where
  showsPrec _ = fst . shows'
    where
      -- |Returns pair @(repr, t)@ where @repr@ represents given expression
      -- and @t@ is used to decide about parentheses.
      shows' :: Regex a -> (String -> String, RegexType)
      shows' Epsilon       = (id, TEpsilon)
      shows' (CharSet set) = (shows set, TAtom)
      shows' (Or a b)      = (wrap' TOr a . ('|':) . wrap' TOr b, TOr)
      shows' (And a b)     = (wrap' TAnd a . ('&':) . wrap' TAnd b, TAnd)
      shows' (Concat a b)  = (wrap' TConcat a . wrap' TConcat b, TConcat)
      shows' (Not r)       = (('^':) . wrap' TNot r, TNot)
      shows' (Capture _ r) = (('(':) . shows r . (')':), TAtom)
      shows' (Counter minRep maxRep r)
        = (wrap' TCounter r . (counterStr ++), TCounter)
        where
          counterStr = '{':minRepStr ++ maxRepStr ++ "}"
          minRepStr  = show minRep
          maxRepStr  = case maxRep of Nothing -> ","
                                      Just maxRep'
                                        | minRep == maxRep' -> ""
                                        | otherwise -> ',':show maxRep'
      -- |If necessary adds parentheses around @r@ so it can be placed
      -- inside @parent@.
      wrap' parent r
        | (parent >= TConcat && parent >= child)
          || (child == TNot && parent == TCounter)
          || child == TAtom
        = repr
        | otherwise = ("(?" ++) . repr . (')':)
        where
          (repr, child) = shows' r
