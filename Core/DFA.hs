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
import Data.Graph (SCC(..), stronglyConnCompR)
import Control.Arrow (second)
import Data.List (partition)
import Core.Utils (sortAndGroupBySnd)

infixl 9 !!!

(!!!) :: (U.IArray UArray b, Ix a) => UArray a b -> a -> b
(!!!) = (U.!)

(///) :: (U.IArray UArray b, Ix a) => UArray a b -> [(a, b)] -> UArray a b
(///) = (U.//)

data StateData a
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
       , sdTrans :: Transitions a
       }

-- | Transitions of one state.
type Transitions a = Pa a

-- | Deterministic finite state automaton.
--
--   Initial state has index 0.
type DFA a = Array State (StateData a)

-- | State number.
type State = Int

-- ---------------------------------------------------------------------------
-- Computation of the highest reachable priority and highest matching rule

-- | Updates 'sdMatches' and 'sdMatchPrio' of the given automaton.
--
--   Parameter @rules@ is same list of the rules which was used when
--   building automaton.
updateWhatMatches :: [Rule a] -> DFA a -> DFA a
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
updateReachablePrio :: Symbol a => DFA a -> DFA a
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
scc :: Symbol a => DFA a -> [SCC (Maybe Priority, State, Neighbours)]
scc = stronglyConnCompR . map convertState . assocs
  where
    convertState (i, sd)
      = (sdMatchPrio sd, i, map fst $ representatives $ sdTrans sd)

-- ---------------------------------------------------------------------------
-- Automaton minimization

-- | Moore's minimization.
kMinimize :: Symbol a => Int -> DFA a -> DFA a
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
initialEquivalence :: DFA a -> Equivalence
initialEquivalence = map (map fst) . sortAndGroupBySnd
                                   . map (second sdMatches) . assocs

-- | Refine equivalence.
--
--   If not refined then returned equivalence is same as original equivalence.
nextEquivalence :: Symbol a
                => Equivalence -> DFA a -> Equivalence
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
kthEquivalence :: Symbol a => Int -> DFA a -> Equivalence
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
mergeEquivStates :: Symbol a => Equivalence -> DFA a -> DFA a
mergeEquivStates eq dfa = removeNotGivenStates reprStates newDFA
  where
    -- Equivalence where state 0 is in the first equivalence class.
    -- State 0 will be first state in the class.
    eq' = let ([x], xs) = partition ((== 0) . head) eq
          in x:xs

    -- Each eq. class is represented by one state.
    reprStates = map head eq'

    -- Transitions of @reprStates@ lead only to other @reprStates@.
    newDFA = dfa // map (\st -> (st, updateTransitions stateToReprState
                                       $ dfa!st))
                        reprStates

    stateToReprState
      = U.array (bounds dfa)
                (concatMap (\eqCls -> zip eqCls (repeat $ head eqCls)) eq')

-- | States not present in the given list will be removed.
removeNotGivenStates :: Symbol a => [State] -> DFA a -> DFA a
removeNotGivenStates states dfa
  = listArray (0, pred $ length states)
              (map (updateTransitions oldToNewSt . (dfa!)) states)
  where
    oldToNewSt :: UArray State State
    oldToNewSt = U.array (bounds dfa) (zip states [0..])

-- | Maps old state numbers in transition table to new state numbers.
updateTransitions ::Symbol a
                  =>  UArray State State -> StateData a -> StateData a
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
data BDFA a
  = BDFA {
           bRegex2Num :: M.Map (RE a) Int
         , bVector2State :: M.Map Vector State
         , bStates :: [(State, StateData a)]
         }

-- | Builds automaton recognizing given rules.
buildDFA :: Symbol a => [Rule a] -> DFA a
buildDFA = toDFA . fst . constructState emptyBDFA . reList
  where
    reList     = map (\(Rule _ _ _ re _) -> toRE re)
    emptyBDFA  = BDFA M.empty M.empty []
    toDFA bdfa = array (0, pred $ M.size $ bVector2State bdfa) (bStates bdfa)

addRE :: Symbol a
      => M.Map (RE a) Int -> RE a -> (M.Map (RE a) Int, Int)
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
constructState :: Symbol a
               => BDFA a -> [RE a] -> (BDFA a, State)
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
buildTransitions :: Symbol a
                 => [RE a] -> BDFA a -> (Transitions a, BDFA a)
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
