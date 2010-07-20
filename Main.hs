{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Environment (getArgs)
import FrontEnd.RuleParser
import BackEnd.CPP
import Control.Applicative ((<$>))
import Core.Partition (PartitionL)

main :: IO ()
main = do rulesFile <- head <$> getArgs
          rulesStr <- readFile rulesFile
          let parsed = parseRules rulesStr
          case parsed of
            Left errMsg -> putStrLn $ "Parsing of rules was not successful: "
                                      ++ show errMsg
            Right rules -> do
              let rules' = map pRule (rules :: [ParsedRule PartitionL])
              putStrLn ""
              putStrLn $ generateCode 800 rules'

{-
This can be useful for debugging:

infixl 9 !!!

(!!!) :: (U.IArray UArray b, Ix a) => UArray a b -> a -> b
(!!!) = (U.!)

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

-}