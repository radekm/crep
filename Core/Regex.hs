-- |
-- Module    : Core.Regex
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Extended regular expressions.
--
module Core.Regex where

import Data.List (intersperse)
import Core.SymbSet

-- |Represents extended regular expression.
data Regex = REpsilon
           | RCharSet CharSet
           | ROr [Regex]
           | RAnd [Regex]
           | RConcat [Regex]
           | RCounter !Laziness !Int !(Maybe Int) Regex
           | RNot Regex
           | RGroup !Int Regex

data Laziness = Greedy | Lazy
              deriving Eq

data Descriptor
  = Epsilon | Atom | ONot | OCounter | OConcat | OAnd | OOr
  deriving (Eq, Ord)

data NaryOperator = OpConcat | OpAnd | OpOr

instance Show Regex where
  showsPrec _ = fst . shows'
    where
      -- Returns a pair @(repr, d)@ where @repr@ is a representation
      -- of given regular expression and @d@ describes shape
      -- of the regular expression.
      shows' :: Regex -> (String -> String, Descriptor)
      shows' REpsilon       = (id,        Epsilon)
      shows' (RCharSet set) = (shows set, Atom)
      shows' (ROr rs)       = showsNary OpOr     rs
      shows' (RAnd rs)      = showsNary OpAnd    rs
      shows' (RConcat rs)   = showsNary OpConcat rs
      shows' (RNot r)       = (('^':) . wrap ONot r,     ONot)
      shows' (RGroup _ r)   = (('(':) . shows r . (')':), Atom)
      shows' (RCounter lazy minRep maxRep r)
        = (wrap OCounter r . (counterStr ++), OCounter)
        where
          counterStr = '{':minRepStr ++ maxRepStr ++ '}':lazyStr
          lazyStr    = if lazy == Lazy then "?" else ""
          minRepStr  = show minRep
          maxRepStr  = case maxRep of Nothing -> ","
                                      Just maxRep'
                                        | minRep == maxRep' -> ""
                                        | otherwise -> ',':show maxRep'
      showsNary OpOr []     = shows' $ RCharSet empty
      showsNary OpAnd []    = shows' $ RCharSet (complement empty)
      showsNary OpConcat [] = shows' $ REpsilon
      showsNary _ [r]      = shows' r
      showsNary op rs
        = ( foldl (.) id $ intersperse opRepr $ map (wrap opDesc) rs
          , opDesc
          )
        where
          opDesc = case op of { OpOr -> OOr 
                              ; OpAnd -> OAnd ; OpConcat -> OConcat }
          opRepr = case op of { OpOr     -> (" | " ++)
                              ; OpAnd    -> (" & " ++) ; OpConcat -> id }
      -- Returns representation of @r@ which can be given as an argument
      -- to operator @parent@. That means represantation of @r@ is taken
      -- and wrapped in parentheses if necessary.
      wrap parent r
        | (parent >= OConcat && parent >= child)
            || (child == ONot && parent == OCounter)
            || child == Atom
          = repr
        | otherwise = ("(?" ++) . repr . (')':)
        where
          (repr, child) = shows' r

