
module Main where

import Data.Array
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import Core.Partition (PartitionL)
import Core.RE ()
import Core.Rule
import Core.DFA
import FrontEnd.RuleParser
import Core.PartialOrder ()
import Control.Applicative ((<$>))

main :: IO ()
main = do rulesFile <- head <$> getArgs
          rulesStr <- readFile rulesFile
          let parsed = parseRules rulesStr
          case parsed of
            Left errMsg -> putStrLn $ "Parsing of rules was not successful: "
                                      ++ show errMsg
            Right rules -> do
              putStrLn "Parsing OK"
              -- Build Brzozowski's automaton and print number of its states.
              putStrLn "Starting Brzozowski's construction..."
              let rules' = map pRule (rules :: [ParsedRule PartitionL])
              let dfa = kMinimize maxBound $ updateReachablePrio
                                           $ updateWhatMatches rules'
                                           $ buildDFA rules'
              let numOfStates = succ $ snd $ bounds $ dfa
              putStrLn $ "Automaton has " ++ show numOfStates ++ " states"
              let reachableSum = sum $ map (\(Pr p) -> p) $ catMaybes
                                     $ map sdReachablePrio
                                     $ elems dfa
              putStrLn $ "Sum of reachable priorities " ++ show reachableSum

{-
import FrontEnd.RuleParser (parseRules)
import Core.Rule (Rule(..), Priority(..))
import Core.RE (toRE)
import Core.DFA (resToDFA, unDFA, state2Int, removeUnreachableStates
                ,computeReachablePrio, removeTransitionsToLowerPrio
                ,kMinimize)
import Control.Applicative ((<$>))

main :: IO ()
main = do rulesFile <- head <$> getArgs
          rulesStr <- readFile rulesFile
          let parsed = parseRules rulesStr
          case parsed of
            Left errMsg -> putStrLn $ "Parsing of rules was not successful: "
                                      ++ show errMsg
            Right rules -> do
              putStrLn "Parsing OK"
              -- Convert list of rules into list of pairs
              -- @(RE Yes, 1)@. For now we ignore priority of the rules.
              let reList = map (\(Rule _ _ _ regex _) ->
                                  (toRE regex, Pr 1))
                               rules
              -- Build Brzozowski's automaton and print number of its states.
              putStrLn "Starting Brzozowski's construction..."
              let dfa = kMinimize maxBound
                          $ removeUnreachableStates
                          $ removeTransitionsToLowerPrio
                          $ computeReachablePrio
                          $ resToDFA reList
              let numOfStates = state2Int (snd $ bounds $ unDFA dfa) + 1
              putStrLn $ "Automaton has " ++ show numOfStates ++ " states"
--              putStrLn $ showDFA dfa

-}