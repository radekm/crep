{-# LANGUAGE GADTs,
             FlexibleContexts #-}

-- |
-- Module    : FrontEnd.RegexParser
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Parser for regular expressions.
--
module FrontEnd.RegexParser
       (
         RegexParserSt(..)
       , p_regex
       , maxRepetitions
       , maxCaptureNum
       ) where

import Prelude hiding (repeat)
import Control.Monad (forM_, liftM, when)
import Control.Arrow ((&&&))

import FrontEnd.Parsec
import Core.Regex
import Core.Set
import Core.Utils

class RegexParserSt a where
  -- | Generates new number for capturing group.
  newCaptureNum :: a -> (Int, a)
  -- | This function is called when capturing group is subexpression
  --   of complement. Content of such group cannot be captured.
  cannotCapture :: Int -> a -> a
  -- | This function is called when the number of capturing groups exceeds
  --   'maxCaptureNum'. Only first 'maxCaptureNum' groups will stay capturing
  --   remaining groups will be non-capturing.
  tooManyCaptures :: a -> a

-- | Parses regular expression.
p_regex :: RegexParserSt st => Parsec String st (Regex Char Yes)
p_regex = p_or

-- | Parses union.
p_or :: RegexParserSt st => Parsec String st (Regex Char Yes)
p_or = safeFoldl1 (error "p_or") Or <$> sepBy1 p_and (char_ '|')

-- | Parses intersection.
p_and :: RegexParserSt st => Parsec String st (Regex Char Yes)
p_and = safeFoldl1 (error "p_and") And <$> sepBy1 p_concat (char_ '&')

-- | Parses concatenation.
p_concat :: RegexParserSt st => Parsec String st (Regex Char Yes)
p_concat = safeFoldl1 Epsilon Concat <$> many p_repeat

-- | Parses negated atom with quantifier.
p_repeat :: RegexParserSt st => Parsec String st (Regex Char Yes)
p_repeat = p_natom <**> option id p_quantifier

-- | Parses quantifier.
p_quantifier :: Parsec String st (Regex Char Yes -> Regex Char Yes)
p_quantifier = choice [ char_ '*' >> return (RepeatU 0)
                      , char_ '+' >> return (RepeatU 1)
                      , char_ '?' >> return (Repeat 0 1)
                      , between (char_ '{') (char_ '}') p_counter
                      ]
  where
    p_counter = do x <- number_ 0 maxRepetitions
                   y <- option (Just x)
                               (char_ ',' >>
                                optionMaybe (number_ x maxRepetitions))
                   return $ maybe (RepeatU x) (\y' -> Repeat x y') y

-- | Parses negated atom.
p_natom :: RegexParserSt st
        => Parsec String st (Regex Char Yes)
p_natom = neg <|> p_atom
  where
    neg = do _ <- char_ '^'
             (r, removed) <- liftM (removeCaptures &&& listCaptures) p_atom
             forM_ removed (modifyState . cannotCapture)
             return $ Not r

-- | Parses atom of regular expression.
p_atom :: RegexParserSt st => Parsec String st (Regex Char Yes)
p_atom = p_char <|> p_any <|> p_class <|> p_group
  where
    p_char = do c <- p_charInRegex
                return $ CharClass $ fromRanges [Range c c]
    p_any = char_ '.' >> return (CharClass alphabet)

-- | Parses capturing and non-capturing group.
p_group :: RegexParserSt st => Parsec String st (Regex Char Yes)
p_group = between (char_ '(') (char_ ')') (p_quest <*> p_regex) <?> "group"
  where
    p_quest = choice [ char_ '?' >> return id
                     , do st <- getState
                          let (num, newSt) = newCaptureNum st
                          if num <= maxCaptureNum
                            then do putState newSt
                                    return $ Capture num
                            -- Too high capture number.
                            else do modifyState tooManyCaptures
                                    return id
                     ]

-- ---------------------------------------------------------------------------
-- Parsing character classes

-- | Parses character class.
p_class :: Parsec String st (Regex Char c)
p_class = between (char_ '[') (char_ ']') (p_caret <*> p_content)
        <?> "character class"
  where
    p_caret   = (CharClass .) <$> option id (char_ '^' >> return complement)
    p_content = fromRanges <$> ((dash <*> many p_range)
                                  <|> (many1 p_range <**> option id dash))
    dash      = char_ '-' >> return (Range '-' '-':)

-- TODO: escape double quotes when showing x or y

-- | Parses one character or range of characters in character class.
p_range :: Parsec String st (Range Char)
p_range = do x <- p_charInClass
             y <- option x (char_ '-' >> notLower x)
             return (Range x y)
  where
    notLower x = try (do y <- p_charInClass
                         when (x > y)
                           $ unexpected ('"': escapeSpecial y ++ "\"")
                         return y)
               <?> "character with code not lower than code of "
                     ++ '"' :  escapeSpecial x ++ "\""

-- ---------------------------------------------------------------------------
-- Parsing characters

-- | Parses one character or escape sequence in regular expression.
p_charInRegex :: Parsec String st Char
p_charInRegex = noneOf_ "\\/[{()|&*+?^." <|> escapeSeq_ <?> "character"

-- | Parses one character or escape sequence in character class.
p_charInClass :: Parsec String st Char
p_charInClass = noneOf_ "\\]-" <|> escapeSeq_ <?> "character"

-- ---------------------------------------------------------------------------
-- Other functions

-- | Maximal allowed value of @n@ in regular expression @r{m,n}@.
maxRepetitions :: Int
maxRepetitions = 999

-- | Maximal number of capturing groups.
maxCaptureNum :: Int
maxCaptureNum = 999  -- Must be at least 2 digits and must have successor.
