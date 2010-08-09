{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : FrontEnd.RuleParser
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Parser for rules.
--
module FrontEnd.RuleParser
       (
         ParsedRule(..)
       , parseRules
       , parseRegex
       ) where

import Control.Monad (liftM2, when)
import Numeric (readDec)
import FrontEnd.RegexParser
import FrontEnd.Parsec
import Core.Regex
import Core.Rule

-- | Parsed rule with additional information.
data ParsedRule
  = P { -- | Rule which was parsed.
        pRule :: Rule Char
        -- | Content of which groups cannot be captured.
        --
        --   Only groups with number not exceeding 'maxCaptureNum'
        --   are listed.
      , pCannotCapture :: [Int]
        -- | Count of capturing groups exceeded 'maxCaptureNum'. Capturing
        --   groups with higher number than 'maxCaptureNum' were treated
        --   as non-capturing.
      , pTooManyCaptures :: Bool
      }

-- ---------------------------------------------------------------------------
-- Manipulation with parser state

-- | State of the rule parser.
data RuleParserState
  = S { -- | Last number which was returned by 'newCaptureNum'.
        _sLastCaptureNum :: Int
        -- | See 'ParsedRule'.
      , sCannotCapture :: [Int]
        -- | See 'ParsedRule'.
      , sTooManyCaptures :: Bool
        -- | First name belongs to the rule which is currently being parsed.
      , _sRuleNames :: [String]
      }

instance RegexParserSt RuleParserState where
  newCaptureNum (S i cc m names)   = (i+1, S (i+1) cc m names)
  cannotCapture c (S i cc m names) = S i (c:cc) m names
  tooManyCaptures (S i cc _ names) = S i cc True names

newState :: RuleParserState
newState = S 0 [] False  []

nextRule :: RuleParserState -> RuleParserState
nextRule (S _ _ _ names) = S 0 [] False names

isNameUsed :: String -> RuleParserState -> Bool
isNameUsed n (S _ _ _ names) = n `elem` names

addName :: String -> RuleParserState -> RuleParserState
addName n (S i cc m names) = S i cc m (n:names)

-- ---------------------------------------------------------------------------
-- Parsing

-- | Parses all rules in the given string.
parseRules :: String -> Either ParseError [ParsedRule]
parseRules = runParser p_rules newState "rules"

-- | Parses given string into regular expression.

-- Note: This function should naturally be part of the 'FrontEnd.RegexParser'
-- module but it uses 'RuleParserState' which is in this module so it is
-- in this module too.
parseRegex :: String -> Either ParseError (Regex Char Yes)
parseRegex = runParser p_regex newState "regular expression"

-- | Parses all rules until EOF.
p_rules :: Parsec String RuleParserState [ParsedRule]
p_rules = skipSpacesAndComments *> many p_rule <* eof

-- | Parses one rule.
p_rule :: Parsec String RuleParserState ParsedRule
p_rule = do modifyState nextRule
            rule <- (Rule <$> p_name <*> p_priority <*>
                              p_flag <*> p_regex' <*> p_subst')
            st <- getState
            return $ P rule (sCannotCapture st) (sTooManyCaptures st)
  where
    p_regex' = between (char_ '/') (char_ '/') p_regex
    p_subst' = p_subst <* char_ '/'

-- | Parses name of the rule.
p_name :: Parsec String RuleParserState Name
p_name = (try (do name <- liftM2 (:) letter $
                                 many (letter <|> digit)
                  st <- getState
                  when (isNameUsed name st) (unexpected name)
                  modifyState (addName name)
                  return name)
          <?> "unique name") <* skipSpacesAndComments

-- | Parses priority of the rule.
p_priority :: Parsec String st Priority
p_priority = Pr <$> number_ 0 maxPriority
           <?> "priority"

-- | Parses flag for the preferred length of the selected words. If no flag
--   is given then @'Shortest'@ is returned.
p_flag :: Parsec String st PrefLen
p_flag = choice [ char_ 's' >> return Shortest
                , char_ 'l' >> return Longest
                , return Shortest ]

-- | Parses substitution.
p_subst :: Parsec String st (Subst Char)
p_subst = Subst <$> many (p_tconst <|> p_tgroup)
  where
    p_tconst
      = TConst <$> many1 (noneOf_ "\\/$" <|> escapeSeq_ <?> "character")
    p_tgroup  = TCapture <$> (char '$' >> p_grpCode <* skipSpacesAndComments)
    -- We assume that maxCaptureNum has more than one digit.
    p_grpCode = (fst . head . readDec . (:"") <$> digit)
              <|> between (char '{') (char '}') (number 0 maxCaptureNum)

maxPriority :: Int
maxPriority = 999
