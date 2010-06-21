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
       , kMinimize
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
import Control.Arrow (second)
import Data.List (groupBy, sortBy)

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
-- Computation of the highest reachable priority and highest matching rule

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
-- Automaton minimization

-- | Moore's minimization.
kMinimize :: (Ord (p a), Pa p a) => Int -> DFA p a -> DFA p a
kMinimize k dfa = mergeEquivStates (kthEquivalence k dfa) dfa

-- | Equivalence class.
type EqClass = [State]

-- | Equivalence is a list of equivalence classes.
type Equivalence = [EqClass]

-- | Maps each state to equivalence class id.
type EqArray = UArray State EqClsId

-- | Id of quivalence class.
type EqClsId = Int

-- | States are grouped by 'sdMatches'.
initialEquivalence :: DFA p a -> Equivalence
initialEquivalence = map (map fst) . sortAndGroupBySnd
                                   . map (second sdMatches) . assocs

-- | Refine equivalence.
--
--   If not refined then returned equivalence is same as original equivalence.
nextEquivalence :: (Ord (p a), Pa p a)
                => Equivalence -> DFA p a -> Equivalence
nextEquivalence eq dfa = concatMap refineEqClass eq
  where
    -- IMPORTANT: Order of states is preserved when class is not subdidived.
    refineEqClass :: EqClass -> [[State]]
    refineEqClass = map (map fst) . sortAndGroupBySnd
                                  . map (second $ pmap (eqArr!!!))
                                  -- Pairs (state, transitions).
                                  . map (\st -> (st, sdTrans $ dfa!st))

    eqArr = equivalenceToEqArray (succ $ snd $ bounds dfa) eq

-- | If @k@ is negative number we loop until equivalence is refined.
kthEquivalence :: (Ord (p a), Pa p a) => Int -> DFA p a -> Equivalence
kthEquivalence k dfa = iter 0 (initialEquivalence dfa)
  where
    -- @equiv@ is @nIter@-th equivalence
    iter nIter equiv
      | nIter == k         = equiv
      | equiv == nextEquiv = equiv
      | otherwise          = iter (succ nIter) nextEquiv
      where
        nextEquiv = nextEquivalence equiv dfa

-- | Every equivalence class is replaced by one state.
mergeEquivStates :: Pa p a => Equivalence -> DFA p a -> DFA p a
mergeEquivStates eq dfa = removeNotGivenStates reprStates newDFA
  where
    -- Each eq. class is represented by one state.
    reprStates = map head eq

    -- Transitions of @reprStates@ lead only to other @reprStates@.
    newDFA = dfa // map (\st -> (st, updateTransitions stateToReprState
                                       $ dfa!st))
                        reprStates

    stateToReprState
      = U.array (bounds dfa)
                (concatMap (\eqCls -> zip eqCls (repeat $ head eqCls)) eq)

-- | States not present in the given list will be removed.
removeNotGivenStates :: Pa p a => [State] -> DFA p a -> DFA p a
removeNotGivenStates states dfa
  = listArray (0, pred $ length states)
              (map (updateTransitions oldToNewSt . (dfa!)) states)
  where
    oldToNewSt :: UArray State State
    oldToNewSt = U.array (bounds dfa) (zip states [0..])

-- | Maps old state numbers in transition table to new state numbers.
updateTransitions ::Pa p a
                  =>  UArray State State -> StateData p a -> StateData p a
updateTransitions oldToNewStMap (SD m mp rp ts)
  = SD m mp rp $ pmap (oldToNewStMap!!!) ts

-- | Conversion between representations of equivalence.
equivalenceToEqArray :: Int -> Equivalence -> EqArray
equivalenceToEqArray nStates eq
  = U.array (0, pred nStates) $ concat $ stateId
  where
    stateId :: [[(State, EqClsId)]]
    stateId = zipWith (\states clsId -> zip states $ repeat clsId)
                      eq [0..]

-- | Sorts the list of pairs by the second value. Then groups values
--   in the list by the second value.
sortAndGroupBySnd :: Ord b => [(a, b)] -> [[(a, b)]]
sortAndGroupBySnd = groupBy (co2 (==) snd) . sortBy (co2 compare snd)
  where
    co2 f t a b = f (t a) (t b)

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
