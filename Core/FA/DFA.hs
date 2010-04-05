{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Core.FA.DFA
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Deterministic finite state automaton.
--
module Core.FA.DFA where

import Prelude hiding (last)
import qualified Data.Map as M
import Data.Graph (SCC(..), dfs, stronglyConnCompR)
import Data.Tree (flatten)
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Core.SymbSet
import Core.Rule
import Core.RE
import Control.Monad.ST (ST, runST)
import Control.Monad
import Control.Arrow ((***), second)

-- |Number of the state.
newtype StNum = StN Int
              deriving (Eq, Ord, Enum, Show, Ix, IArray UArray)

state2Int :: StNum -> Int
state2Int (StN i) = i

data StateData a
  = SD {
         -- List of the rules which match in this state.
         sdMatches :: [RuNum]
         -- Priority of the rules which match in this state.
       , sdMatchPrio :: Maybe Priority
         -- Highest priority of the rule which is reachable from this state
         -- by some non-empty word.
       , sdReachablePrio :: Maybe Priority
         -- Transitions from this state. Every symbol appears just in one
         -- symbol set and every state appears at most once.
       , sdTrans :: [(SymbSet a, StNum)]
       }

newtype DFA a = DFA (Array StNum (StateData a))

unDFA :: DFA a -> Array StNum (StateData a)
unDFA (DFA a) = a

-- |Makes deterministic finite state automaton from regular expressions.
resToDFA :: [(RE, Priority)] -> DFA Char
resToDFA rsPrio = toDFA rsPrio $ brzoCons rsPrio

-- |Returns list with reachable states. First state in the list must
-- be initial state.
reachable :: DFA a -> [StNum]
reachable dfa = map StN $ flatten $ head $ dfs graph [0]
  where
    dfa'   = unDFA dfa
    (a, b) = (state2Int *** state2Int) (bounds dfa')
    graph  = listArray (a, b) [map (state2Int . snd)
                                   (sdTrans $ dfa' ! StN i) | i <- [a..b]]

-- |Removes states which are not reachable from the initial state. Remaining
-- states are renumbered.
removeUnreachableStates :: DFA a -> DFA a
removeUnreachableStates dfa = DFA 
    $ listArray (StN 0, last) [updateStNums $ dfa' ! i | i <- [StN 0..last]]
  where
    dfa' = unDFA dfa
    rs   = reachable dfa
    last = StN $ length rs - 1

    newStNums :: UArray StNum StNum
    newStNums = accumArray (flip const) (StN (-1)) (bounds dfa')
                                                   (zip rs [StN 0..])

    -- Updates state numbers in transitions inside StateData.
    updateStNums (SD ms m r ts) = SD ms m r $ map (second (newStNums !)) ts

-- |Strongly connected components topologically sorted.
scc :: DFA a -> [SCC (Maybe Priority, StNum, [StNum])]
scc = stronglyConnCompR . map convertState . assocs . unDFA
  where
    convertState (i, st) = (sdMatchPrio st, i, map snd $ sdTrans st)

computeReachablePrio :: DFA a -> DFA a
computeReachablePrio dfa
  = DFA $ listArray bnds [newStateData i | i <- uncurry enumFromTo bnds]
  where
    dfa' = unDFA dfa
    bnds = bounds dfa'
    cs   = scc dfa

    defPr                  = minBound
    fromPrio (Just (Pr i)) = i
    fromPrio _             = defPr

    newStateData i = let sd    = dfa' ! i
                         newPr = maxReachablePrio ! i
                     in sd {sdReachablePrio = if newPr /= defPr
                                                then Just $ Pr newPr
                                                else Nothing}

    maxReachablePrio :: UArray StNum Int
    maxReachablePrio = runST computeMaxReachablePrioForEachState

    computeMaxReachablePrioForEachState :: ST s (UArray StNum Int)
    computeMaxReachablePrioForEachState
      = do maxPrio <- (newArray bnds defPr :: ST s (STUArray s StNum Int))
           forM_ cs (handleSCC maxPrio)
           unsafeFreeze maxPrio

    handleNeighbours a (n:ns)
      = do inReach <- readArray a n
           rest    <- handleNeighbours a ns
           return $ maximum [inReach, rest, fromPrio (sdMatchPrio $ dfa' ! n)]
    handleNeighbours _ _
      = return defPr
                                   
    handleSCC a (CyclicSCC xs)
      = do curMax <- foldM handleVertex defPr xs
           forM_ xs $ \(_, st, _) -> writeArray a st curMax
      where
        handleVertex curMax (pr, _, ns)
          = do neighbours <- handleNeighbours a ns
               return $ maximum [curMax, neighbours, fromPrio pr]

    handleSCC a (AcyclicSCC (_, st, ns))
      = do neighbours <- handleNeighbours a ns
           writeArray a st neighbours

-- |Converts list to unboxed array.
toU :: IArray UArray a => Int -> [a] -> UArray Int a
toU len xs = listArray (0, len - 1) xs


------------------------------------------------------------------------------
-- Brzozowski's construction of DFA
--
-- We use idea described in article
-- "Regular-expression derivatives reexamined" by Scott Owens, John Reppy and
-- Aaron Turon.

-- |Regular expression number.
newtype RENum = REN Int
              deriving (Eq, Ord, Show, IArray UArray)

-- |Structure which represents data of the state in Brzozowski's automaton.
data BrzoStateData = BSD { 
                           -- State number.
                           bsdStNum :: !StNum
                           -- Transitions of the state. Character sets form
                           -- partition of the alphabet.
                         , bsdTrans :: [(CharSet, StNum)]
                           -- List of rules which match in this state.
                         , bsdMatches :: [RuNum]
                         }
                   deriving Show

-- |Representation of partial automaton in Brzozowski's construction.
--
-- States in Brzozowski's construction are normally represented by vectors
-- of regular expressions. But we will represent them by vectors of numbers
-- (@UArray Int RENum@). Each number in the vector represents regular
-- expression. The main advantage of this representation is that we can
-- compare two vectors of numbers faster than two vectors of regular
-- expressions.
--
-- To associate number with regular expression we use map @bREs@. When
-- the regular expression is in the map we use the number from the map
-- otherwise we add it to the map with some unique number.
-- 
-- In transitions we don't use vectors of numbers for state representation
-- instead we associate each vector with its unique number of type @StNum@.
-- Associations of vectors with their numbers are hold in @bStates@.
-- (This makes conversion from @BrzoDFA@ to @DFA@ easier.)
--
-- The transitions of each state are saved into @bStData@ list.
-- Each state is represented by one item in the list. The item consists
-- of the state number, transitions and list of all rules which match
-- in this state.
--
-- For each state construction algorithm computes which rules match in that
-- state. When some rule matches it has no sense to ask whether some other
-- rule with lower priority matches. And so we need to determine which rules
-- should we examine whether they match. For this purpose we use function
-- @bWhatCanMatch@ which is called @bWhatCanMatch dfa r@ where @dfa@ is the
-- partial automaton and @r@ is the lowest number of the rule which matches.
-- The function returns number of the rule @r2@ and all rules between @r@
-- and @r2@ should be examined.
--
-- (Rule with lower number must not have lower priority than the rule with
-- higher number -- i.e. rules are ordered by priority.)
data BrzoDFA
  = BDFA {
           -- Maps regular expressions to numbers. 
           bREs :: M.Map RE RENum
           -- Maps state vectors to state numbers.
         , bStates :: M.Map (UArray Int RENum) StNum
           -- Data of states.
         , bStData :: [BrzoStateData]
           -- Which rules can match.
         , bWhatCanMatch :: RuNum -> RuNum
           -- Count of the rules.
         , bVectLength :: Int
         }

-- |Constructs Brzozowski's automaton for given regular expressions.
brzoCons :: [(RE, Priority)] -> BrzoDFA
brzoCons rsPrio
  = fst $ buildBrzoState (BDFA mapREs M.empty [] wcmFunc len)
                         (toU len reNums) rs
  where
    (rs, ps) = unzip rsPrio
    len   = length rsPrio
    -- Rule numbers and corresponding priorities.
    rsNum = reverse $ zip [0..] ps
    -- What can match function.
    wcmFunc (RuN i) = RuN $ toU len [ let prio = ps !! j
                                      -- Highest number of the rule which
                                      -- has priority @prio@.
                                      in fst $ head
                                             $ filter ((== prio) . snd) rsNum
                                    | j <- [0..(len-1)]] ! i
    -- Add regular expressions to the map.
    (mapREs, reNums) = addREList M.empty rs

-- |Adds regular expression to the map. Returns new map and number of regular
-- expression.
addRE :: M.Map RE RENum -> RE -> (M.Map RE RENum, RENum)
addRE mapREs r = case oldRENum of
                   Just n -> (mapREs, n)
                   -- Regular expression @r@ was not in the map.
                   _      -> (newMapREs, newRENum)
  where
    -- Number if @r@ is not in @mapREs@.
    newRENum = REN (M.size mapREs)
    -- Inserts @r@ into @mapREs@. If @r@ isn't there, returns
    -- @(Nothing, newREs)@ where @newREs@ is a map with @r@ added otherwise
    -- returns @(Just n, _)@ where @n@ is the number of @r@ from the map.
    (oldRENum, newMapREs) = M.insertLookupWithKey (\_ _ old -> old) r
                              newRENum mapREs

-- |Adds all regular expressions in the list to the map and returns their
-- numbers.
addREList :: M.Map RE RENum -> [RE] -> (M.Map RE RENum, [RENum])
addREList mapREs = second reverse . foldl addOneRE (mapREs, [])
  where
    addOneRE (oldREs, reNumList) r
      = let (newREs, reNum) = addRE oldREs r
        in (newREs, reNum:reNumList)

-- |Builds state of Brzozowski's automaton and returns partial automaton
-- with the state and state number. State is considered to be built when it
-- is inside @bStates@ map.
--
-- If the state is not in the map we build its transitions and all states
-- which are reachable from this state.
buildBrzoState :: BrzoDFA -> UArray Int RENum -> [RE] -> (BrzoDFA, StNum)
buildBrzoState dfa stVect reList
  = case oldStNum of
      Just stNum -> (dfa, stNum)
      -- State is not in the automaton. We have to construct it.
      Nothing -> (newDFA, newStNum)
  where
    newStNum = StN $ M.size (bStates dfa)
    (oldStNum, newStates) = M.insertLookupWithKey (\_ _ old -> old) stVect
                              newStNum (bStates dfa)

    -- Which rules match in the state.
    newMatches = case matches of
                   m:ms -> m:takeWhile (<= bWhatCanMatch dfa m) ms
                   _    -> []
      where
        matches = map fst $ filter snd $ zip [(RuN 0)..] $ map nullable reList
    
    alphabetPartition = partitionAlphabetByDerivativesMany reList

    -- Create outgoing transitions. For each block of the @alphabetPartition@
    -- we build destination state and transition to that state. We have to add
    -- current state into the map to prevent building it again when calling
    -- @buildBrzoState@ recursively.
    (dfa', newTrans) = foldl buildNextState (dfa {bStates = newStates}, [])
                         (fromPartition alphabetPartition)
      where
        -- 
        buildNextState (auto, finTrans) blockOfPart
          = (newAuto, (blockOfPart, stateNum):finTrans)
          where
            -- Derivative of list with regular expressions.
            reList' = map (derivative $ firstSymb blockOfPart) reList
            -- Create @stList'@ with numbers of regular expressions. Regular
            -- expression which were not in @bREs auto@ are added to @newREs@.
            (newREs, stList') = addREList (bREs auto) reList'
            -- Vector with regular expression numbers.
            stVect' = toU (bVectLength dfa) (reverse stList')
            -- Add regular expressions to automaton.
            auto' = auto {bREs=newREs} 

            (newAuto, stateNum) = buildBrzoState auto' stVect' reList'

    newDFA = dfa' {bStData = BSD {bsdStNum=newStNum, bsdTrans=newTrans
                                 ,bsdMatches=newMatches}:bStData dfa'}

-- |Converts BrzoDFA to DFA.
toDFA :: [(RE, Priority)] -> BrzoDFA -> DFA Char
toDFA rsPrio brzoDFA
  = DFA $ array
      (StN 0, StN $ M.size (bStates brzoDFA) - 1) $
      map (\st -> let matches                = bsdMatches st
                      getMatchPrio (RuN m:_) = Just $ snd (rsPrio !! m)
                      getMatchPrio _         = Nothing
                  in (bsdStNum st, SD matches (getMatchPrio matches) Nothing
                                      (bsdTrans st)))
          (bStData brzoDFA)

