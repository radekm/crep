{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module    : Core.FA.DFA
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Deterministic finite state automaton.
--
module Core.FA.DFA where

import qualified Data.Map as M
import Data.Array.Vector hiding (lengthU, indexU)
import Data.Array.Vector.UArr (lengthU, indexU)
import Core.SymbSet
import Core.Partition
import Core.Rule
import Core.RE
import Control.Applicative ((<$>))
import Control.Arrow (second)

-- |Number of the state.
newtype StNum = StN Int
              deriving (Eq, Ord, Show)

------------------------------------------------------------------------------
-- Brzozowski's construction of DFA
--
-- We use idea described in article
-- "Regular-expression derivatives reexamined" by Scott Owens, John Reppy and
-- Aaron Turon.

-- |Regular expression number.
newtype RENum = REN Int
              deriving (Eq, Ord, Show)

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
-- (@UArr RENum@). Each number in the vector represents regular expression.
-- The main advantage of this representation is that we can compare two
-- vectors of numbers faster than two vectors of regular expressions.
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
         , bStates :: M.Map (UArr RENum) StNum
           -- Data of states.
         , bStData :: [BrzoStateData]
           -- Which rules can match.
         , bWhatCanMatch :: RuNum -> RuNum
         , bVectLength :: Int
         }

-- |Constructs Brzozowski's automaton for given regular expressions.
brzoCons :: [(RE, Priority)] -> BrzoDFA
brzoCons rsPrio
  = fst $ buildBrzoState (BDFA mapREs M.empty [] wtmFunc len)
                         (toU reNums) rs
  where
    (rs, ps) = unzip rsPrio
    len   = length rsPrio
    -- Priorities and corresponding "rule numbers".
    rsNum = zip [0..] ps
    -- What can match function.
    wtmList = map (\i -> fst $ last $ filter ((== (ps !! i)) . snd) rsNum)
                  [0..(len-1)]
    wtmFunc (RuN i) = RuN $ indexU (toU wtmList) i
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
addREList :: M.Map RE RENum -> [RE]
          -> (M.Map RE RENum, [RENum])
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
buildBrzoState :: BrzoDFA -> UArr RENum -> [RE] -> (BrzoDFA, StNum)
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
                         (toCharSets alphabetPartition)
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
            stVect' = toU (reverse stList')
            -- Add regular expressions to automaton.
            auto' = auto {bREs=newREs} 

            (newAuto, stateNum) = buildBrzoState auto' stVect' reList'

    newDFA = dfa' {bStData = BSD {bsdStNum=newStNum, bsdTrans=newTrans
                                 ,bsdMatches=newMatches}:bStData dfa'}

instance UA RENum where
  newtype UArr  RENum   = UARENum (UArr Int)
  newtype MUArr RENum s = MUARENum (MUArr Int s)

  lengthU (UARENum a) = lengthU a
  indexU (UARENum a) = REN . indexU a
  sliceU (UARENum a) i = UARENum . sliceU a i

  lengthMU (MUARENum a) = lengthMU a
  newMU i = MUARENum <$> newMU i
  readMU (MUARENum a) i = REN <$> readMU a i
  writeMU (MUARENum a) i (REN x) = writeMU a i x
  copyMU (MUARENum dest) i (UARENum src) = copyMU dest i src
  unsafeFreezeMU (MUARENum a) i = UARENum <$> unsafeFreezeMU a i

  memcpyMU     (MUARENum a) (MUARENum b) = memcpyMU a b
  memcpyOffMU  (MUARENum a) (MUARENum b) = memcpyOffMU a b
  memmoveOffMU (MUARENum a) (MUARENum b) = memmoveOffMU a b

instance Ord (UArr RENum) where
  -- We assume that both vectors have same length.
  compare a b = cmp 0 (lengthU a)
    where
      cmp i len | i < len   = case compare (indexU a i) (indexU b i) of
                                EQ -> cmp (i+1) len
                                r  -> r
                | otherwise = EQ

