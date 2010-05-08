-- |
-- Module    : FrontEnd.RuleParser
-- Copyright : (c) Radek Micek 2009-2010
-- License   : BSD3
-- Stability : experimental
--
-- Parser for rules.
--
module FrontEnd.RuleParser
    (
      parseRules
    , parseRegex
    ) where

import Control.Monad (liftM2, when)
import Numeric (readDec)
import FrontEnd.RegexParser
import FrontEnd.Parsec
import Core.Regex
import Core.Rule

-- |State of the rule parser.
data RuleParserState = S Int [Int] [String]

instance RegexParserSt RuleParserState where
    newGroupNum (S i remCs names)     = (i+1, S (i+1) remCs names)
    cannotCapture c (S i remCs names) = S i (c:remCs) names

newState :: RuleParserState
newState = S 0 [] []

nextRule :: RuleParserState -> RuleParserState
nextRule (S _ _ names) = S 0 [] names

nameUsed :: String -> RuleParserState -> Bool
nameUsed n (S _ _ names) = n `elem` names

addName :: String -> RuleParserState -> RuleParserState
addName n (S i remCs names) = S i remCs (n:names)

-- |Parses all rules in the given string.
parseRules :: String -> Either ParseError [Rule]
parseRules = runParser p_rules newState "rules"

-- |Parses given string to regular expression.
--
-- Note: This function should naturally be part of the 'FrontEnd.RegexParser'
-- module but it uses 'RuleParserState' which is in this module so it is
-- in this module too.
parseRegex :: String -> Either ParseError (Regex Yes)
parseRegex = runParser p_regex newState "regular expression"

-- |Parses all rules until EOF.
p_rules :: Parsec String RuleParserState [Rule]
p_rules = skipSpacesAndComments *> many p_rule <* eof

-- |Parses one rule.
p_rule :: Parsec String RuleParserState Rule
p_rule = do modifyState nextRule
            (Rule <$> p_name <*> p_priority <*>
                      p_flag <*> p_regex' <*> p_subst')
  where
    p_regex' = between (char_ '/') (char_ '/') p_regex
    p_subst' = p_subst <* char_ '/'

-- |Parses name of the rule.
p_name :: Parsec String RuleParserState Name
p_name = (try (do name <- liftM2 (:) letter $
                                 many (letter <|> digit)
                  st <- getState
                  when (nameUsed name st) (unexpected name)
                  modifyState (addName name)
                  return name)
          <?> "unique name") <* skipSpacesAndComments

-- |Parses priority of the rule.
p_priority :: Parsec String st Priority
p_priority = Pr <$> number_ 0 maxPriority
           <?> "priority"

-- |Parses flag for the preferred length of the selected words. If no flag
-- is given then @'Shortest'@ is returned.
p_flag :: Parsec String st PrefLength
p_flag = choice [ char_ 's' >> return Shortest
                , char_ 'l' >> return Longest
                , return Shortest ]

-- |Parses substitution.
p_subst :: Parsec String st Subst
p_subst = Subst <$> many (p_tconst <|> p_tgroup)
  where
    p_tconst
      = TConst <$> many1 (noneOf_ "\\/$" <|> escapeSeq_ <?> "character")
    p_tgroup  = TCapture <$> (char '$' >> p_grpCode)
    -- We assume that maxCaptureNum has more than one digit.
    p_grpCode = (fst . head . readDec . (:"") <$> digit)
              <|> between (char '{') (char '}') (number 0 maxCaptureNum)

maxPriority :: Int
maxPriority = 999

