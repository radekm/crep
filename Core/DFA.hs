{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module    : Core.DFA
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Deterministic finite state automaton.
--
module Core.DFA
       (
         DFA
       , State
       , StateData(..)
       , Transitions
       , updateWhatMatches
       , updateReachablePrio
       , buildDFA
       ) where

import qualified Data.Map as M
import Data.Array
import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed (UArray)
import Core.Rule
import Core.RE
import Data.Maybe (fromJust)
import Data.List (mapAccumL)
import Core.Partition
import Data.Monoid (Monoid)
import Data.Graph (SCC(..), stronglyConnCompR)

infixl 9 !!!

(!!!) :: (U.IArray UArray b, Ix a) => UArray a b -> a -> b
(!!!) = (U.!)

(///) :: (U.IArray UArray b, Ix a) => UArray a b -> [(a, b)] -> UArray a b
(///) = (U.//)

data StateData p a
  = SD {
         -- | List of the rules which match in this state.
         sdMatches :: [RuNum]
         -- | Priority of the maximal rule which match in this state.
       , sdMatchPrio :: Maybe Priority
         -- | Priority of the maximal rule which is reachable from this state
         --   by some non-empty word.
       , sdReachablePrio :: Maybe Priority
         -- | Transition table of the state is represented as partition
         --   of the alphabet. Block ids represent destination states.
       , sdTrans :: Transitions p a
       }

-- | Transitions of one state.
type Transitions p a = p a

-- | Deterministic finite state automaton.
--
--   Initial state has index 0.
type DFA p a = Array State (StateData p a)

-- | State number.
type State = Int

-- ---------------------------------------------------------------------------
--

-- | Updates 'sdMatches' and 'sdMatchPrio' of the given automaton.
--
--   Parameter @rules@ is same list of the rules which was used when
--   building automaton.
updateWhatMatches :: [Rule p a] -> DFA p a -> DFA p a
updateWhatMatches rules dfa
  = listArray (bounds dfa) $ map updateSD $ elems dfa
  where
    ru2Prio :: UArray RuNum Priority
    ru2Prio = U.listArray (RuN 0, RuN $ pred $ length rules) $
                          map (\(Rule _ p _ _ _) -> p) rules
    updateSD (SD matches _ rp ts)
      = case matches of
          [] -> SD [] Nothing rp ts
          _  -> SD newMatches (Just maxPrio) rp ts
      where
        maxPrio    = maximum $ map (ru2Prio!!!) matches
        newMatches = filter ((== maxPrio) . (ru2Prio!!!)) matches

-- | Updates 'sReachablePrio' of the given automaton.
--
--   'sdMatchPrio' must be already updated.
updateReachablePrio :: Pa p a => DFA p a -> DFA p a
updateReachablePrio dfa = array (bounds dfa) $ map updateState $ assocs dfa
  where
    updateState (st, sd)
      = (st, sd {sdReachablePrio = toMPrio $ finReachablePrioArr!!!st})
    finReachablePrioArr = foldl maxOfSCC initReachablePrioArr $ scc dfa

    -- Maps each state to the highest priority which can be reached
    -- by non-empty word. @minBound@ means that no priority can be reached.
    initReachablePrioArr :: UArray State Priority
    initReachablePrioArr = U.accumArray const minBound (bounds dfa) []

    -- Adds states from one strongly connected component into the map
    -- of reachable priorities.
    maxOfSCC :: UArray State Priority
             -> SCC (Maybe Priority, State, Neighbours)
             -> UArray State Priority
    maxOfSCC reachablePrioArr (CyclicSCC xs)
      = reachablePrioArr /// zip (map (\(_, st, _) -> st) xs) (repeat maxPrio)
      where
        maxPrio = maximum $ map maxOfState xs
        maxOfState (pr, _, ns) = max (fromMPrio pr)
                                     (maxOfNeighbours reachablePrioArr ns)
    maxOfSCC reachablePrioArr (AcyclicSCC (_, st, ns))
      = reachablePrioArr /// [(st, maxOfNeighbours reachablePrioArr ns)]

    maxOfNeighbours :: UArray State Priority -> Neighbours -> Priority
    maxOfNeighbours reachablePrioArr neighbours
      = maximum $ map maxOfNeighbour neighbours
      where
        maxOfNeighbour nst = max (fromMPrio $ sdMatchPrio $ dfa!nst)
                                 (reachablePrioArr!!!nst)

    fromMPrio (Just p) = p
    fromMPrio _        = minBound

    toMPrio p
      | p /= minBound = Just p
      | otherwise     = Nothing

type Neighbours = [State]

-- | Returns strongly connected components topologically sorted.
scc :: Pa p a => DFA p a -> [SCC (Maybe Priority, State, Neighbours)]
scc = stronglyConnCompR . map convertState . assocs
  where
    convertState (i, sd)
      = (sdMatchPrio sd, i, map fst $ representatives $ sdTrans sd)


-- ---------------------------------------------------------------------------
-- DFA construction Brzozowski
--
-- We use idea described in article
-- "Regular-expression derivatives reexamined" by Scott Owens, John Reppy
-- and Aaron Turon.

-- | Vector representing state.
type Vector = [Int]

-- | States are represented by vectors of regular expressions.
--   We map regular expressions to integers and vectors of integers to state
--   numbers and so we represent state by numbers.
data BDFA p a
  = BDFA {
           bRegex2Num :: M.Map (RE p a) Int
         , bVector2State :: M.Map Vector State
         , bStates :: [(State, StateData p a)]
         }

-- | Builds automaton recognizing given rules.
buildDFA :: (Monoid (p a), Ord (p a), Pa p a) => [Rule p a] -> DFA p a
buildDFA = toDFA . fst . constructState emptyBDFA . reList
  where
    reList     = map (\(Rule _ _ _ re _) -> toRE re)
    emptyBDFA  = BDFA M.empty M.empty []
    toDFA bdfa = array (0, pred $ M.size $ bVector2State bdfa) (bStates bdfa)

addRE :: Ord (p a)
      => M.Map (RE p a) Int -> RE p a -> (M.Map (RE p a) Int, Int)
addRE m re = case M.insertLookupWithKey f key newVal m of
               (Just reNum, _)   -> (m, reNum)
               (Nothing, newMap) -> (newMap, newVal)
  where
    f _key _newVal oldVal = oldVal
    key    = re
    newVal = M.size m

addVector :: Vector
          -> M.Map Vector State
          -> (State, Maybe (M.Map Vector State))
addVector vect m = case M.insertLookupWithKey f key newVal m of
                     (Just st, _)      -> (st, Nothing)
                     (Nothing, newMap) -> (newVal, Just newMap)
  where
    f _key _newVal oldVal = oldVal
    key    = vect
    newVal = M.size m

-- | Function @'constructState' bdfa reList@ returns new automaton with
--   state given by @reList@.
constructState :: (Monoid (p a), Ord (p a), Pa p a)
               => BDFA p a -> [RE p a] -> (BDFA p a, State)
constructState bdfa reList
  = case maybeVect2State of
      Nothing
        -> (bdfa, st)  -- State is already in automaton.
      Just _
        -> (bdfa'' { bStates = (st, stateData):(bStates bdfa'') }, st)
  where
    -- Map regular expressions.
    (regex2Num, vector) = mapAccumL addRE (bRegex2Num bdfa) reList
    -- Map vector.
    (st, maybeVect2State) = addVector vector (bVector2State bdfa)

    bdfa' = BDFA regex2Num (fromJust maybeVect2State) (bStates bdfa)
    (transitions, bdfa'') = buildTransitions reList bdfa'
    stateData = SD whatMatches Nothing Nothing transitions
    whatMatches = map fst $ filter snd $ zip [RuN 0..] $ map nullable reList

-- | Function @'buildTransitions' reList bdfa@ returns transitions
--   for the state given by @reList@ and new automaton @bdfa'@ where all
--   states reachable from state given by @reList@ are constructed.
--
--   @bdfa@ contains state given by @reList@. That means that regular
--   expressions in @reList@ are also in @'bRegex2Num' bdfa@ and vector
--   representing state is in @'bVector2Num' bdfa@.
buildTransitions :: (Monoid (p a), Ord (p a), Pa p a)
                 => [RE p a] -> BDFA p a -> (Transitions p a, BDFA p a)
buildTransitions reList bdfa = (transitions, bdfa')
  where
    pa = partitionAlphabetByDerivativesMany reList
    (blocks, symbols) = unzip $ representatives pa
    -- @states@ is list with destination states.
    (bdfa', states)
      = mapAccumL (\dfa s -> constructState dfa $ map (derivative s) reList)
                  bdfa symbols
    block2State :: UArray BlockId State
    block2State = U.array (0, maximum blocks) (zip blocks states)
    -- Replace block ids by states.
    transitions = pmap (block2State!!!) pa
