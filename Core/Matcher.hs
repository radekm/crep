{-# LANGUAGE FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses #-}

-- |
-- Module    : Core.Matcher
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- Finds matching prefixes of the given string.
--
module Core.Matcher
       (
         Matcher(..)
       , Length
       , BinSearchMatcher(..)
       , BTransitionTab
       , CompAlphabetMatcher(..)
       , TranslationTable
       , TransitionTable
       , TabIdx
       , TSymbol
       , toBinSearchMatcher
       , toCompAlphabetMatcher
       ) where

import Core.Rule
import Core.DFA
import Data.Array
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.Monoid
import Core.Partition
import Data.List (partition)
import Data.Word (Word8)
import Core.Utils

infixl 9 !!!

(!!!) :: (U.IArray UArray b, Ix a) => UArray a b -> a -> b
(!!!) = (U.!)

type Length = Int

class Matcher m s where
  findWords :: m s -> [s] -> [(RuNum, [Length])]

instance Matcher BinSearchMatcher Char where
  findWords = findWordsBSM

instance Matcher BinSearchMatcher Word8 where
  findWords = findWordsBSM

findWordsBSM :: (Ord s, U.IArray UArray s)
             => BinSearchMatcher s -> [s] -> [(RuNum, [Length])]
findWordsBSM bsm
  = findWordsGeneric fWhatMatches fMatchPrio fReachablePrio fNextState
  where
    fWhatMatches       = (bsmWhatMatches bsm!)
    fMatchPrio         = (bsmMatchPrio bsm!)
    fReachablePrio     = (bsmReachablePrio bsm!)
    fNextState st symb = ttStates !!! (binSearch lo hi symb ttSymbols)
      where
        (ttSymbols, ttStates) = bsmTransitionTabs bsm ! st
        (lo, hi)              = U.bounds ttSymbols

instance Matcher CompAlphabetMatcher Char where
  findWords = findWordsCAM

instance Matcher CompAlphabetMatcher Word8 where
  findWords = findWordsCAM

findWordsCAM :: Ix s => CompAlphabetMatcher s -> [s] -> [(RuNum, [Length])]
findWordsCAM cam
  = findWordsGeneric fWhatMatches fMatchPrio fReachablePrio fNextState
  where
    fWhatMatches       = (camWhatMatches cam!)
    fMatchPrio         = (camMatchPrio cam!)
    fReachablePrio     = (camReachablePrio cam!)
    fNextState st symb = (camTransitionTabs cam ! st) !!! translSymb
      where
        translTabIdx = camSymbolTranslation cam !!! st
        translTab    = camTranslationTabs cam ! translTabIdx
        translSymb   = translTab !!! symb

findWordsGeneric :: forall s. (State -> [RuNum])
                 -> (State -> Maybe Priority)
                 -> (State -> Maybe Priority)
                 -> (State -> s -> State)
                 -> [s]
                 -> [(RuNum, [Length])]
findWordsGeneric fWhatMatches fMatchPrio fReachablePrio fNextState
  = map (\xs -> (snd $ head xs, map fst xs)) .
    sortAndGroupBySnd .
    concat .
    runDFA 0 1 Nothing
  where
    runDFA :: State {- old state -}
           -> Length
           -> Maybe Priority
           -> [s]
           -> [[(Length, RuNum)]]
    runDFA _ _ _ [] = []
    runDFA st len maxPrio (symb:symbols)
      | reachablePrio < maxPrio' = [whatMatches]
      | otherwise = whatMatches:runDFA newState (succ len) maxPrio' symbols
      where
        whatMatches = zip (repeat len) $ fWhatMatches newState
        reachablePrio = fReachablePrio st
        maxPrio' = maxPrio `max` (fMatchPrio st)
        newState = fNextState st symb

-- | Function @'binSearch' lo hi symb arr@ returns index of the smallest
--   element @el@ such that @symb <= el@.
--
--   Element @el@ must exist.
binSearch :: (U.IArray UArray a, Ord a)
          => Int -> Int -> a -> UArray Int a -> Int
binSearch lo hi symb arr
  | lo == hi         = lo
  -- Element is too small.
  | arr!!!mid < symb = binSearch (mid+1) hi  symb arr
  | otherwise        = binSearch lo      mid symb arr
  where
    mid  = (lo + hi) `div` 2

-- --------------------------------------------------------------------------

-- | Representation of transition table. The first array is search by binary
--   search and the second array contains next state.
type BTransitionTab a = (UArray Int a, UArray Int State)

data BinSearchMatcher a
  = BSM {
        -- | Transition tables.
          bsmTransitionTabs :: Array State (BTransitionTab a)
        -- | Which rules match.
        , bsmWhatMatches :: Array State [RuNum]
        -- | Highest priority which matches.
        , bsmMatchPrio :: Array State (Maybe Priority)
        -- | Highest priority reachable by some nonempty word.
        , bsmReachablePrio :: Array State (Maybe Priority)
        }

toBinSearchMatcher :: (U.IArray UArray a, Pa p a)
                   => DFA p a -> BinSearchMatcher a
toBinSearchMatcher dfa
  = BSM (fmap (listsToTransTab . unzip . toList . sdTrans) dfa)
        (fmap sdMatches dfa)
        (fmap sdMatchPrio dfa)
        (fmap sdReachablePrio dfa)
  where
    listsToTransTab (sts, symbols) = (U.listArray bnds symbols,
                                      U.listArray bnds sts)
      where
        bnds = (0, pred $ length symbols)

-- ---------------------------------------------------------------------------
-- Alphabet compression.
--
-- We use algorithm described in article
-- "Efficient Signature Matching with Multiple Alphabet Compression Tables"
-- by Shijin Kong, Randy Smith and Cristian Estan

-- | Translation of symbols.
type TranslationTable a = UArray a TSymbol

-- | Transition table for one state.
type TransitionTable = UArray TSymbol State

-- | Index of translation table.
type TabIdx = Int

-- | Translated symbol.
type TSymbol = Int

data CompAlphabetMatcher a
  = CAM {
        -- | Tables for symbol translation.
          camTranslationTabs :: Array TabIdx (TranslationTable a)
        -- | Transition tables.
        , camTransitionTabs  :: Array State TransitionTable
        -- | Indices of the table for symbol translation.
        , camSymbolTranslation :: UArray State TabIdx
        -- | Which rules match.
        , camWhatMatches :: Array State [RuNum]
        -- | Highest priority which matches.
        , camMatchPrio :: Array State (Maybe Priority)
        -- | Highest priority reachable by some nonempty word.
        , camReachablePrio :: Array State (Maybe Priority)
        }

type NewPartition = [State]
type Rest = [State]

-- | Converts automaton to matcher.
toCompAlphabetMatcher :: (Enum a, Ix a, Pa p a, Monoid (p a))
                      => Int -> DFA p a -> CompAlphabetMatcher a
toCompAlphabetMatcher numPartitions dfa
  = CAM (listArray (0, pred numPartitions) $ map fst tabs)
        (array (bounds dfa) (concatMap snd tabs))
        (U.array (bounds dfa) $ concatMap (\(i, sts) -> zip sts (repeat i))
                              $ zip [0..] statePartition)
        (fmap sdMatches dfa)
        (fmap sdMatchPrio dfa)
        (fmap sdReachablePrio dfa)
  where
    tabs = map (\states ->
                  let (transl, invTransl) = mkTranslationTab states
                      transitions = [ (s, mkTransitionTab s invTransl)
                                    | s <- states]
                  in (transl, transitions))
               statePartition

    -- Returns two tables (symbos -> tsymbol, tsymbol -> symbol).
    mkTranslationTab states
      = (U.array (minBound, maxBound) $
                 concatMap (\(b, u, v) -> [(symbol, b) | symbol <- [u..v]]) $
                           intervals,
         U.array (0, lastTSymbol)
                 -- Works only in GHC since indices may repeat.
                 [(b, u) | (b, u, _) <- intervals])
      where
        intervals   = toIntervals alphaPartit
        alphaPartit = mconcat $ map (sdTrans . (dfa!)) states
        lastTSymbol = fst $ maximum (toList alphaPartit)

    mkTransitionTab st invTranslTab
      = U.array (bounds invTranslTab)
                [ (t, getBlock s $ sdTrans $ dfa!st)
                | (t, s) <- U.assocs invTranslTab]

    -- Each block in partition of states will have its translation table.
    statePartition :: [[State]]
    statePartition
      = let (x, xs) =
              until (\(_, ps) -> length ps + 1 >= numPartitions)
                    (\(sts, ps) -> let (p, sts') = extractOnePartition sts
                                   in (sts', p:ps))
                    (dfaStates, [])
        in x:xs

    dfaStates = let (lo, hi) = bounds dfa in [lo..hi]

    -- Size of partition <= (size of remaining) / 2
    extractOnePartition :: [State] -> (NewPartition, Rest)
    extractOnePartition remaining
      = rmFromPartit (length remaining `div` 2) remaining []
      where
        rmFromPartit maxSize part rest
          | length part > maxSize = let (cut, r) = statesCut part
                                    in rmFromPartit maxSize r (cut ++ rest)
          | otherwise             = (part, rest)

        -- Returns (cut, rest).
        statesCut states
          -- Partition of the alphabet has only one block.
          | null statePartitions = (states, [])
          | otherwise            = snd $ minimum statePartitionsWithLen
          where
            -- [(symbol, symbol)]
            pairsOfSymbs = combinations2 $ map snd
                                         $ representatives
                                         $ mconcat
                                         $ map (sdTrans . (dfa!)) states
            -- List of pairs (states where symbols behave differently, rest).
            statePartitions = map (\p -> partition (behavesDifferently p)
                                                   states)
                                  pairsOfSymbs
              where
                behavesDifferently (a, b) state
                  = let ts = sdTrans $ dfa!state
                    in getBlock a ts /= getBlock b ts
            statePartitionsWithLen = map (\a -> (length $ fst a, a))
                                         statePartitions

combinations2 :: [a] -> [(a, a)]
combinations2 []     = []
combinations2 (x:xs) = [(x, y) | y <- xs] ++ combinations2 xs
