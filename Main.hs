
module Main where

import Core.Partition ()
import Core.RE ()
import Core.Rule ()
import FrontEnd.RegexParser ()

main :: IO ()
main = return ()

{-
import System.Environment (getArgs)
import FrontEnd.RuleParser (parseRules)
import Core.Rule (Rule(..), Priority(..))
import Core.RE (toRE)
import Core.DFA (resToDFA, unDFA, state2Int, removeUnreachableStates
                ,computeReachablePrio, removeTransitionsToLowerPrio
                ,kMinimize)
import Control.Applicative ((<$>))
import Data.Array

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