-- |
-- Module    : FrontEnd.RuleParser
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Parser for rules.
--
module FrontEnd.RuleParser where

import Numeric (readDec)
import FrontEnd.RegexParser
import FrontEnd.Parsec
import Core.Regex
import Core.Rule

-- |State of the rule parser.
data RuleParserState = S Int [Int]

instance RegexParserSt RuleParserState where
  newGroupNum (S i remCs)     = (i+1, S (i+1) remCs)
  cannotCapture c (S i remCs) = S i (c:remCs)

newState :: RuleParserState
newState = (S 0 [])

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
p_rule = do putState newState
            (Rule <$> p_priority <*> p_flag <*> p_regex' <*> p_subst')
  where
    p_regex' = between (char_ '/') (char_ '/') p_regex
    p_subst' = p_subst <* char_ '/'

-- |Parses priority of the rule.
p_priority :: Parsec String st Priority
p_priority = Pr <$> number_ 0 maxPriority

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
    p_tgroup  = TGroup <$> (char '$' >> p_grpCode)
    -- We assume that maxCaptureNum has more than one digit.
    p_grpCode = (fst . head . readDec . (:"") <$> digit)
              <|> between (char '{') (char '}') (number 0 maxCaptureNum)

maxPriority :: Int
maxPriority = 999

