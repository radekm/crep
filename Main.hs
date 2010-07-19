{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Array
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import Core.Partition (PartitionL, toList)
import Core.RE ()
import Core.Rule
import Core.DFA
import Core.Matcher
import FrontEnd.RuleParser
import Core.PartialOrder ()
import Control.Applicative ((<$>))
import Core.UTF8 ()
import Core.Capture ()
import BackEnd.CPP

infixl 9 !!!

(!!!) :: (U.IArray UArray b, Ix a) => UArray a b -> a -> b
(!!!) = (U.!)

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
                  maCAM = toCompAlphabetMatcher 5 {- alpa partitions -} dfa
                  maBSM = toBinSearchMatcher dfa
              putStrLn $ show $ map (toList . sdTrans) $ elems $ dfa
              let numOfStates = succ $ snd $ bounds $ dfa
              putStrLn $ "Automaton has " ++ show numOfStates ++ " states"
              let reachableSum = sum $ map (\(Pr p) -> p) $ catMaybes
                                     $ map sdReachablePrio
                                     $ elems dfa
              putStrLn $ "Sum of reachable priorities " ++ show reachableSum
--            putStrLn $ show $ camTranslationTabs matcher ! 1 !!! 'a'
              putStrLn $ show $ findWords maCAM "acbd"
              putStrLn $ show $ findWords maBSM "acbd"
--            putStrLn $ show $ elems $ camWhatMatches $ matcher
              putStrLn ""
              putStrLn $ generateCode 800 rules'
