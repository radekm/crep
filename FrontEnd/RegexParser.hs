-- |
-- Module    : FrontEnd.RegexParser
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Parser for regular expressions.
--
module FrontEnd.RegexParser where

import Control.Monad (when)

import FrontEnd.Parsec
import Core.Regex
import Core.SymbSet
import Core.Utils

class RegexParserSt a where
  newGroupNum :: a -> (Int, a)
  resetGroupNum :: a -> a

-- |Parses regular expression.
p_regex :: RegexParserSt st => Parsec String st Regex
p_regex = p_or

-- |Parses union.
p_or :: RegexParserSt st => Parsec String st Regex
p_or = ROr <$> sepBy1 p_and (char_ '|')

-- |Parses intersection.
p_and :: RegexParserSt st => Parsec String st Regex
p_and = RAnd <$> sepBy1 p_concat (char_ '&')

-- |Parses concatenation.
p_concat :: RegexParserSt st => Parsec String st Regex
p_concat = RConcat <$> many p_repeat

-- |Parses negated atom with quantifier.
p_repeat :: RegexParserSt st => Parsec String st Regex
p_repeat = p_natom <**> option id p_quantifier

-- |Parses quantifier.
p_quantifier :: Parsec String st (Regex -> Regex)
p_quantifier = choice [ char_ '*' >> return RStar
                      , char_ '+' >> return plus
                      , char_ '?' >> return quest
                      , between (char_ '{') (char_ '}') p_counter
                      ] <*> p_lazy
  where
    p_lazy = option Greedy (char_ '?' >> return Lazy)
    plus Lazy r    = RConcat [r, RStar Lazy r]
    plus Greedy r  = RConcat [r, RStar Greedy r]
    quest Lazy r   = ROr [REpsilon, r]
    quest Greedy r = ROr [r, REpsilon]
    p_counter = do x <- number_ 0 999
                   y <- option (Just x)
                               (char_ ',' >> optionMaybe (number_ x 999))
                   return $ \lazy -> RCounter lazy x y

-- |Parses negated atom.
p_natom :: RegexParserSt st => Parsec String st Regex
p_natom = option id (char_ '^' >> return RNot) <*> p_atom

-- |Parses atom of regular expression.
p_atom :: RegexParserSt st => Parsec String st Regex
p_atom = p_char <|> p_any <|> p_set <|> p_group
  where
    p_char = do c <- p_charInRegex
                return $ RCharSet $ fromRanges [mkRange c c]
    p_any = char_ '.' >> return (RCharSet alphabet)

-- PROBLEM: there is no check for overflow of group numbers

-- |Parses capturing and non-capturing group.
p_group :: RegexParserSt st => Parsec String st Regex
p_group = between (char_ '(') (char_ ')') (p_quest <*> p_regex) <?> "group"
  where
    p_quest = choice [ char_ '?' >> return id
                     , do st <- getState
                          let (num, newSt) = newGroupNum st
                          putState newSt
                          return $ RGroup num
                     ]

------------------------------------------------------------------------------
-- Parsing character sets

-- |Parses character set.
p_set :: Parsec String st Regex
p_set = between (char_ '[') (char_ ']') (p_caret <*> p_content)
      <?> "character class"
  where
    p_caret   = (RCharSet .) <$> option id (char_ '^' >> return complement)
    p_content = fromRanges <$> ((dash <*> many p_range)
                                  <|> (many1 p_range <**> option id dash))
    dash      = char_ '-' >> return (mkRange '-' '-':)

-- TODO: escape double quotes when showing x or y

-- |Parses one character or range of characters in character set.
p_range :: Parsec String st (Range Char)
p_range = do x <- p_charInSet
             y <- option x (char_ '-' >> notLower x)
             return (mkRange x y)
  where
    notLower x = try (do y <- p_charInSet
                         when (x > y)
                           $ unexpected ('"': escapeSpecial y ++ "\"")
                         return y)
               <?> "character with code not lower than code of "
                     ++ '"' :  escapeSpecial x ++ "\""

------------------------------------------------------------------------------
-- Parsing characters

-- |Parses one character or escape sequence in regular expression.
p_charInRegex :: Parsec String st Char
p_charInRegex = noneOf_ "\\/[{()|&*+?^." <|> escapeSeq_ <?> "character"

-- |Parses one character or escape sequence in character set.
p_charInSet :: Parsec String st Char
p_charInSet = noneOf_ "\\]-" <|> escapeSeq_ <?> "character"

