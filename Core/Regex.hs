
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
           | ROr [Regex]
           | RAnd [Regex]
           | RConcat [Regex]
           | RNot Regex
           | RStar !Laziness Regex
           | RCounter !Laziness !Int !(Maybe Int) Regex
           | RGroup !Int Regex

data Laziness = Greedy | Lazy

