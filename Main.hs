{-# LANGUAGE FlexibleContexts, 
             DeriveDataTypeable #-}
module Main where

import FrontEnd.RuleParser
import BackEnd.CPP
import Core.Partition (PartitionL)
import System.Console.CmdArgs

data CrepArgs = CArgs {
                        maxWordLen :: Integer
                      , rulesFile :: String
                      , outputFile :: String
                      }
              deriving (Show, Data, Typeable)

crepArgs :: Mode CrepArgs
crepArgs
  = mode $ CArgs {
                   maxWordLen = 1024
                                &= text "Maximal length of matching words"
                                & explicit
                                & flag "k"
                                & typ "NUMBER"
                 , rulesFile = def
                               &= typ "rules-file"
                               & argPos 0
                               & text "a"
                 , outputFile = def
                                &= typ "output-file"
                                & argPos 1
                                & text "b"
                 } &= text ("Generates C++ program for processing text " ++
                            "according to the given rules.")

wordLenBnds :: (Integer, Integer)
wordLenBnds = (1, 512 * 1024)

main :: IO ()
main = do a <- cmdArgs "crep 0.1, (C) 2009-2010 Radek Micek" [crepArgs]
          if maxWordLen a < fst wordLenBnds || maxWordLen a > snd wordLenBnds
            then putStrLn $"Maximal length of matching words must be between "
                           ++ show (fst wordLenBnds) ++ " and "
                           ++ show (snd wordLenBnds)
            else do
              rulesStr <- readFile (rulesFile a)
              let parsed = parseRules rulesStr
              case parsed of
                Left errMsg
                  -> putStrLn $"Parsing of rules was not successful: "
                               ++ show errMsg
                Right rs
                  -> do let rules = map pRule (rs :: [ParsedRule PartitionL])
                        writeFile (outputFile a)
                          $ generateCode (fromInteger $ maxWordLen a) rules

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