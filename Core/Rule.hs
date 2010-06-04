{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Core.Rule
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Rules.
--
module Core.Rule where

import Core.Regex

-- | Rule consists of name, priority, flag for preferred length of selected
--   parts, regular expression and substitution.
data Rule p a = Rule Name !Priority !PrefLen (Regex p a Yes) Subst

-- | Name of the rule.
type Name = String

-- | Priority of the rule.
newtype Priority = Pr Int
                 deriving (Eq, Ord, Show)

-- | Flag whether the rule prefers shortest or longest parts.
data PrefLen = Shortest | Longest
             deriving Show

-- | Substitution consists of terms each term refers to the text captured
--   by regular expression or it is constant string.
newtype Subst = Subst [SubstTerm]
              deriving Show

-- | Terms of the substitution.
data SubstTerm = TConst String
               | TCapture !Int
               deriving Show

-- | Number of the rule.
newtype RuNum = RuN Int
              deriving (Eq, Ord, Show, Enum)
