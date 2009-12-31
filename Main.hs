
module Main where

import System.Environment (getArgs)
import FrontEnd.RuleParser (parseRules)
import Core.Rule (Rule(..), Priority(..))
import Core.FA.RE (toRE, simplify)
import Core.FA.DFA (BrzoDFA(..), brzoCons)
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
              let reList = map (\(Rule _ _ regex _) ->
                                  (simplify $ toRE regex, Pr 1))
                               rules
              -- Build Brzozowski's automaton and print number of its states.
              putStrLn "Starting Brzozowski's construction..."
              let brzoDFA = brzoCons reList
              let numOfStates = length $ bStData brzoDFA
              putStrLn $ "Automaton has " ++ show numOfStates ++ " states"

