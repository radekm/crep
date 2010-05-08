{-# LANGUAGE
      GADTs #-}

-- |
-- Module    : FrontEnd.RegexParser
-- Copyright : (c) Radek Micek 2009-2010
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

import Control.Monad (forM_, liftM, when)
import Control.Arrow (first, second)

import FrontEnd.Parsec
import Core.Regex
import Core.SymbSet
import Core.Utils

class RegexParserSt a where
    newGroupNum :: a -> (Int, a)
    cannotCapture :: Int -> a -> a

-- |Parses regular expression.
p_regex :: RegexParserSt st => Parsec String st (Regex Yes)
p_regex = p_or

-- |Parses union.
p_or :: RegexParserSt st => Parsec String st (Regex Yes)
p_or = wrap (error "p_or") Or <$> sepBy1 p_and (char_ '|')

-- |Parses intersection.
p_and :: RegexParserSt st => Parsec String st (Regex Yes)
p_and = wrap (error "p_and") And <$> sepBy1 p_concat (char_ '&')

-- |Parses concatenation.
p_concat :: RegexParserSt st => Parsec String st (Regex Yes)
p_concat = wrap Epsilon Concat <$> many p_repeat

-- |Parses negated atom with quantifier.
p_repeat :: RegexParserSt st => Parsec String st (Regex Yes)
p_repeat = p_natom <**> option id p_quantifier

-- |Parses quantifier.
p_quantifier :: Parsec String st (Regex a -> Regex a)
p_quantifier = choice [ char_ '*' >> return (Counter 0 Nothing)
                      , char_ '+' >> return (Counter 1 Nothing)
                      , char_ '?' >> return (Counter 0 $ Just 1)
                      , between (char_ '{') (char_ '}') p_counter
                      ]
  where
    p_counter = do x <- number_ 0 maxRepetitions
                   y <- option (Just x)
                               (char_ ',' >>
                                optionMaybe (number_ x maxRepetitions))
                   return $ Counter x y

-- |Parses negated atom.
p_natom :: RegexParserSt st => Parsec String st (Regex Yes)
p_natom = neg <|> p_atom 
  where
    neg = do _ <- char_ '^'
             (r, removed) <- liftM removeCaptures p_atom
             forM_ removed (modifyState . cannotCapture)
             return $ Not r

    -- Returns new regular expression and list of removed captures.
    removeCaptures :: Regex a -> (Regex No, [Int])
    removeCaptures r
      = case r of
          Epsilon       -> (Epsilon, [])
          CharSet cs    -> (CharSet cs, [])
          Or  a b       -> two Or     a b
          And a b       -> two And    a b
          Concat a b    -> two Concat a b
          Counter l h a -> one (Counter l h) a
          Not a         -> one Not           a
          Capture i a   -> second (i:) $ removeCaptures a
      where
        two cons a b = let (a', as) = removeCaptures a
                           (b', bs) = removeCaptures b
                       in (cons a' b', as ++ bs)
        one cons = first cons . removeCaptures

-- |Parses atom of regular expression.
p_atom :: RegexParserSt st => Parsec String st (Regex Yes)
p_atom = p_char <|> p_any <|> p_set <|> p_group
  where
    p_char = do c <- p_charInRegex
                return $ CharSet $ fromRanges [mkRange c c]
    p_any = char_ '.' >> return (CharSet alphabet)

-- |Parses capturing and non-capturing group.
p_group :: RegexParserSt st => Parsec String st (Regex Yes)
p_group = between (char_ '(') (char_ ')') (p_quest <*> p_regex) <?> "group"
  where
    p_quest = choice [ char_ '?' >> return id
                     , do st <- getState
                          let (num, newSt) = newGroupNum st
                          if num <= maxCaptureNum
                            then do putState newSt
                                    return $ Capture num
                            -- Too high capture number.
                            else do modifyState (cannotCapture num)
                                    return id
                     ]

------------------------------------------------------------------------------
-- Parsing character sets

-- |Parses character set.
p_set :: Parsec String st (Regex a)
p_set = between (char_ '[') (char_ ']') (p_caret <*> p_content)
      <?> "character class"
  where
    p_caret   = (CharSet .) <$> option id (char_ '^' >> return complement)
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

------------------------------------------------------------------------------
-- Other functions

maxRepetitions :: Int
maxRepetitions = 999

-- Must be at least 2 digits and must have successor.
maxCaptureNum :: Int
maxCaptureNum = 999

