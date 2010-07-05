{-# LANGUAGE GADTs #-}

-- |
-- Module    : Core.Capture
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- Capturing subwords.
module Core.Capture
       (
         CaptureId
       , WordRef
       , CaptLang(..)
       , Gen
       , genCapture
       ) where

import Control.Monad.State
import Core.Regex

-- | Id of captured subword.
type CaptureId = Int

-- | Identifies subwords in computation.
type WordRef = Int

-- | Language for representing computations which extract captured subwords.
data CaptLang p s
  = IfNonEmpty WordRef [CaptLang p s]
  | IfMatches WordRef (Regex p s Yes) [CaptLang p s] [CaptLang p s]
  | SplitWord WordRef (Regex p s Yes) (Regex p s Yes) (WordRef, WordRef)
  | CaptureWord WordRef CaptureId

-- | Monad where generator runs.
type Gen = State WordRef

-- | If word is nonempty.
ifNonEmpty :: WordRef -> Gen [CaptLang p s] -> Gen [CaptLang p s]
ifNonEmpty ref trueBranch
  = do trueCode <- trueBranch
       return [IfNonEmpty ref trueCode]

-- | If regular expression matches word.
ifMatches :: WordRef
          -> Regex p s Yes
          -> Gen [CaptLang p s]  -- ^ Matches branch.
          -> Gen [CaptLang p s]  -- ^ Doesn't match branch.
          -> Gen [CaptLang p s]
ifMatches ref regex trueBranch falseBranch
  = do trueCode <- trueBranch
       falseCode <- falseBranch
       return [IfMatches ref regex trueCode falseCode]

-- | Splits word @w@ into words @u@ and @v@ matching given regular expressions
--   where @u@ is the longest possible word such that @uv = w@.
splitWord :: WordRef
          -> Regex p s Yes  -- ^ Matches word @u@.
          -> Regex p s Yes  -- ^ Matches word @v@.
          -> Gen ([CaptLang p s], WordRef, WordRef)
splitWord ref regex1 regex2
  = do lastRef <- get
       let u = succ lastRef
           v = succ u
       put v
       return ([SplitWord ref regex1 regex2 (u, v)], u, v)

-- | Captures word.
captureWord :: WordRef -> CaptureId -> Gen [CaptLang p s]
captureWord ref i = return [CaptureWord ref i]

-- | Generates computation which extracts captured words.
genCapture :: WordRef -> Regex p s Yes -> Gen [CaptLang p s]
genCapture _ r
  | null $ listCaptures r = return []
genCapture ref (Or a b)
  = ifNonEmpty ref
    (
      ifMatches ref a
      (genCapture ref a)
      (genCapture ref b)
    )
genCapture ref (And a b)
  = ifNonEmpty ref
    (
      do code1 <- genCapture ref a
         code2 <- genCapture ref b
         return (code1 ++ code2)
    )
genCapture ref (Concat a b)
  = ifNonEmpty ref
    (
      do (c, u, v) <- splitWord ref a b
         code1 <- genCapture u a
         code2 <- genCapture v b
         return (c ++ code1 ++ code2)
    )
genCapture ref (RepeatU lo r)
  = genCapture ref (Concat r rest)
  where
    rest | lo > 0    = conv $ RepeatU (pred lo) r
         | otherwise = conv $ RepeatU lo        r
    conv = toRegexWithCaptures . removeCaptures
genCapture ref (Repeat lo hi r)
  | hi > 0    = genCapture ref (Concat r rest)
  | otherwise = return []  -- Word must be empty.
  where
    rest | lo > 0    = conv $ Repeat (pred lo) (pred hi) r
         | otherwise = conv $ Repeat lo        (pred hi) r
    conv = toRegexWithCaptures . removeCaptures
genCapture ref (Capture i r)
  = ifNonEmpty ref
    (
      do code1 <- captureWord ref i
         code2 <- genCapture ref (toRegexWithCaptures r)
         return (code1 ++ code2)
    )
genCapture _ (Not _)       = error "Core.Capture.genCapture: case Not"
genCapture _ Epsilon       = error "Core.Capture.genCapture: case Epsilon"
genCapture _ (CharClass _) = error "Core.Capture.genCapture: case CharClass"
