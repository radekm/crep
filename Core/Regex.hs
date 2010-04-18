-- |
-- Module    : Core.Regex
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Extended regular expressions.
--
module Core.Regex where

import Core.SymbSet

-- |Represents extended regular expression.
data Regex = REpsilon
           | RCharSet CharSet
           | ROr Regex Regex
           | RAnd Regex Regex
           | RConcat Regex Regex
           | RCounter !Laziness !Int !(Maybe Int) Regex
           | RNot Regex
           | RGroup !Int Regex

data Laziness = Greedy | Lazy
              deriving Eq

-- |Converts list of regular expressions to one regular expression.
wrap :: Regex -> (Regex -> Regex -> Regex) -> [Regex] -> Regex
wrap emptyRegex _    []      = emptyRegex
wrap _          _    [x]     = x
wrap _          cons [x, x'] = cons x x'
wrap emptyRegex cons (x:xs)  = cons x (wrap emptyRegex cons xs)

data Descriptor
  = Epsilon | Atom | ONot | OCounter | OConcat | OAnd | OOr
  deriving (Eq, Ord)

instance Show Regex where
  showsPrec _ = fst . shows'
    where
      -- Returns a pair @(repr, d)@ where @repr@ is a representation
      -- of given regular expression and @d@ describes shape
      -- of the regular expression.
      shows' :: Regex -> (String -> String, Descriptor)
      shows' REpsilon       = (id, Epsilon)
      shows' (RCharSet set) = (shows set, Atom)
      shows' (ROr a b)      = (wrap' OOr a . ('|':) . wrap' OOr b, OOr)
      shows' (RAnd a b)     = (wrap' OAnd a . ('&':) . wrap' OAnd b, OAnd)
      shows' (RConcat a b)  = (wrap' OConcat a . wrap' OConcat b, OConcat)
      shows' (RNot r)       = (('^':) . wrap' ONot r, ONot)
      shows' (RGroup _ r)   = (('(':) . shows r . (')':), Atom)
      shows' (RCounter lazy minRep maxRep r)
        = (wrap' OCounter r . (counterStr ++), OCounter)
        where
          counterStr = '{':minRepStr ++ maxRepStr ++ '}':lazyStr
          lazyStr    = if lazy == Lazy then "?" else ""
          minRepStr  = show minRep
          maxRepStr  = case maxRep of Nothing -> ","
                                      Just maxRep'
                                        | minRep == maxRep' -> ""
                                        | otherwise -> ',':show maxRep'
      -- Returns representation of @r@ which can be given as an argument
      -- to operator @parent@. That means represantation of @r@ is taken
      -- and wrapped in parentheses if necessary.
      wrap' parent r
        | (parent >= OConcat && parent >= child)
            || (child == ONot && parent == OCounter)
            || child == Atom
          = repr
        | otherwise = ("(?" ++) . repr . (')':)
        where
          (repr, child) = shows' r

