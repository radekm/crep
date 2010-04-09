{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Core.Rule
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Rules.
--
module Core.Rule where

import Core.Regex

-- |Rule consists of the priority, flag for preferred length of selected
-- words, regular expression and substitution.
data Rule = Rule !Priority !PrefLength Regex Subst
          deriving Show

-- |Priority of the rule.
newtype Priority = Pr Int
                 deriving (Eq, Ord, Show)

-- |Flag whether the rule prefers to select shortest or longest words.
data PrefLength = Shortest | Longest
                deriving Show

-- |Substitution consists of terms each terms refers to the text captured
-- by a group from regular expression or it is constant string.
newtype Subst = Subst [SubstTerm]
              deriving Show

-- |Terms of the substitution.
data SubstTerm = TConst String
               | TGroup !Int
               deriving Show

-- |Number of the rule.
newtype RuNum = RuN Int
              deriving (Eq, Ord, Show, Enum)

