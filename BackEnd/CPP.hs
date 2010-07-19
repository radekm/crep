{-# LANGUAGE FlexibleContexts,
             GADTs,
             QuasiQuotes #-}

-- |
-- Module    : BackEnd.CPP
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- Generates C++ code.
--
module BackEnd.CPP where

import Core.Rule
import Data.Array
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Core.DFA (buildDFA, kMinimize, updateReachablePrio, updateWhatMatches)
import Core.Matcher
import Data.List (intersperse)
import Core.Partition
import Data.Word (Word8)
import Core.UTF8 (convertRule, convertRegex, convertSubst)
import Core.Capture
import Control.Monad.State
import Data.Maybe (catMaybes)
import Core.Regex
import Control.Applicative ((<$>))
import Core.PartialOrder
import BackEnd.Verbatim

-- TODO: support for flags.

-- | Generates C++ code.
generateCode :: Int -> [Rule PartitionL Char] -> String
generateCode k rules = unlines [
                                 beforeAutomaton
                               , generateRuleNames rules
                               , mainPart k rules
                               , betweenAutomatonAndSubsts
                               , generatePOrder rules
                               , capturePart rules
                               , afterSubsts
                               ]

-- | Generates code for main automaton.
--
--   The automaton consists of:
--
-- * one or more translation tables,
--
-- * transition table for each state,
--
-- * table which rule match for each state,
--
-- * table which maps states to their translation tables,
--
-- * table which maps states to their transition tables,
--
-- * table which contains number of matching rules for each state,
--
-- * table which maps state to the table with rules which match there
--
-- * and table which maps states to highest reachable priority.
--
--   In addition we generate 2 tables for rules: one with priorities
--   and one with information which rules prefer shortest words and which
--   prefer longest.
--
--   First parameter is maximal length of the words which will be matched.
mainPart ::  Int -> [Rule PartitionL Char] -> String
mainPart k rules = unlines $ filter (/= "")
                   ([defK, defCntRules, defShortest, defPriorities] ++
                    translTabs ++ transitTabs ++ whatMatchesTabs ++
                    [st2WhatMatchesArr, st2WhatMatchesLen, st2TranslTab
                    ,st2TransitTab, st2ReachablePrio])
  where
    defK          = "#define K " ++ show k
    cntRules      = length rules
    defCntRules   = "#define CNT_RULES " ++ show cntRules

    lstShortest   = map (\(Rule _ _ s _ _) -> s) rules
    lstPriorities = map (\(Rule _ p _ _ _) -> p) rules
    defShortest   = buildTableL lstShortest "bool shortest"
                    (\sh -> case sh of { Shortest -> "true" ; _ -> "false" })
    defPriorities = buildTableL lstPriorities "int priorities"
                    (\(Pr p) -> show p)

    rules' :: [Rule PartitionL Word8]
    rules' = map convertRule rules

    dfa           = kMinimize k' $ updateReachablePrio
                                 $ updateWhatMatches rules'
                                 $ buildDFA rules'
    -- This is because of UTF8.
    k'            = if maxBound `div` 4 > k then k*4 else maxBound
    matcher       = toCompAlphabetMatcher 4 {- alphabet partitions -} dfa

    translTabs    = map createTab $ assocs $ camTranslationTabs matcher
      where
        createTab (i, arr) = buildTableU arr ("unsigned char mTranslTab"
                                                ++ show i) show

    transitTabs   = map createTab $ assocs $ camTransitionTabs matcher
      where
        createTab (st, arr) = buildTableU arr ("StateNum mTransitTab"
                                                 ++ show st) show

    whatMatchesTabs = map createTab $ assocs $ camWhatMatches matcher
      where
        -- No rule matches.
        createTab (_, [])  = ""
        createTab (st, rs) = buildTableL rs ("RuNum mWhatMatches" ++ show st)
                                         (\(RuN r) -> show r)

    st2WhatMatchesArr = buildTableL (assocs $ camWhatMatches matcher)
                          "RuNum * mSt2WhatMatchesArr"
                          (\(st, whatMatches) ->
                            if null whatMatches
                              then "NULL"
                              else "mWhatMatches" ++ show st)

    st2WhatMatchesLen = buildTable (camWhatMatches matcher)
                          "size_t mSt2WhatMatchesLength"
                          (show . length)

    st2TranslTab      = buildTableU (camSymbolTranslation matcher)
                                    "unsigned char * mSt2TranslTab"
                                     (("mTranslTab" ++) . show)
    st2TransitTab     = buildTableL (indices $ camTransitionTabs matcher)
                                    "StateNum * mSt2TransitTab"
                                    (("mTransitTab" ++) . show)
    st2ReachablePrio  = buildTable (camReachablePrio matcher)
                                   "int mSt2ReachablePrio"
                                   (\p -> case p of Just (Pr v) -> show v
                                                    Nothing     -> "-1")

-- ---------------------------------------------------------------------------
-- Creation of substitution and capturing subwords

-- | Generates code of the function @computeSubst@ which gets rule number
--   and output stream and writes substitution into given output stream.
capturePart ::  [Rule PartitionL Char] -> String
capturePart rules = unlines (map (uncurry captureOneRule) numberedRules ++
                             substFunc)
  where
    numberedRules = zip [RuN 0..] rules
    substFunc     = ["void computeSubst(RuNum rule, ostream &output) {"
                    ,"switch(rule) {"
                    ] ++
                    map (\(RuN n, _) -> "case " ++ show n ++ ": subst_" ++
                                        show n ++ "(output); break;")
                        numberedRules ++
                    ["}", "}"]

data GenerateI = GMatches (Regex PartitionL Char Yes)
               | GSplitWord (Regex PartitionL Char Yes)
                            (Regex PartitionL Char Yes)

-- | Describes function which will be generated
--   (which rule, function number, type of the function).
type GenFunc = (RuNum, Int, GenerateI)

-- | Generates function @subst_i@ where @i@ is the number of the rule
--   which writes substitution for the rule @i@ into output stream.
captureOneRule :: RuNum -> Rule PartitionL Char -> String
captureOneRule ruNum (Rule _ _ _prefLen regex subst)
  = unlines [generatedFunctions, generatedMainFunction]
  where
    -- Functions for regex matching.
    generatedFunctions = unlines $ map generateFunctionCode funcsToGen

    -- Function which writes substitution to the output stream.
    generatedMainFunction = unlines [header, decls, bodyCode, substCode, "}"]
      where
        header    = "void subst_" ++ show r ++ "(ostream &output) {"
          where
            (RuN r) = ruNum

        decls     = unlines ["size_t wStart0=0;"
                            ,"size_t wLen0=capVect.size();"
                            ,captDecls]
        captDecls = unlines $
                    map (\i -> let s = show i
                               in "size_t cStart" ++ s ++ "=0;" ++
                                  "size_t cLen"   ++ s ++ "=0;") $
                    capturesInSubst subst'
        bodyCode  = fst $ runState (generateCaptLangCode captLangCode)
                                   (ruNum, 0)
        substCode = generateSubstCode subst'

    funcsToGen :: [GenFunc]
    funcsToGen = fst $ runState (functionsToGen captLangCode) 0

    functionsToGen :: [CaptLang PartitionL Char] -> State Int [GenFunc]
    functionsToGen [] = return []
    functionsToGen (IfNonEmpty _w as:xs)
      = do fs1 <- functionsToGen as
           fs2 <- functionsToGen xs
           return (fs1 ++ fs2)
    functionsToGen (IfMatches _w re as bs:xs)
      = do funcId <- succ <$> get
           put funcId
           fs1 <- functionsToGen as
           fs2 <- functionsToGen bs
           fs3 <- functionsToGen xs
           return ((ruNum, funcId, GMatches re) : fs1 ++ fs2 ++ fs3)
    functionsToGen (SplitWord _w re1 re2 (_wOut1, _wOut2):xs)
      = do funcId <- succ <$> get
           put funcId
           fs <- functionsToGen xs
           return ((ruNum, funcId, GSplitWord re1 re2) : fs)
    functionsToGen (CaptureWord _w _i:xs) = functionsToGen xs

    -- Code for capture extraction.
    captLangCode = fst $ runState (genCapture 0 regex') 0 {- lastRef -}

    -- Regular expression without unused captures.
    regex' = rmCaptures regex
      where
        rmCaptures :: Regex PartitionL Char c -> Regex PartitionL Char c
        rmCaptures Epsilon = Epsilon
        rmCaptures r@(CharClass _) = r
        rmCaptures (Or a b) = rmCaptures a `Or` rmCaptures b
        rmCaptures (And a b) = rmCaptures a `And` rmCaptures b
        rmCaptures (Concat a b) = rmCaptures a `Concat` rmCaptures b
        rmCaptures (RepeatU lo a) = RepeatU lo (rmCaptures a)
        rmCaptures (Repeat lo hi a) = Repeat lo hi (rmCaptures a)
        rmCaptures r@(Not _) = r
        rmCaptures (Capture i a)
          | i `elem` usedCaptures = Capture i (rmCaptures a)
          | otherwise             = toRegexWithCaptures $ rmCaptures a

        usedCaptures = capturesInSubst subst

    -- Substitution without captures not in regular expression.
    subst' = Subst $ rmCaptures terms
      where
        rmCaptures [] = []
        rmCaptures (t@(TCapture i):ts)
          | i `elem` definedCaptures = t:rmCaptures ts
          | otherwise                =   rmCaptures ts
        rmCaptures (t@(TConst _):ts) = t:rmCaptures ts

        (Subst terms) = subst
        definedCaptures  = listCaptures regex

-- | Generates functions for splitting words and matching words.
--   Tables for automata are generated too.
--
--   For actual matching these functions call @genericSplitWord@ and
--   @genericMatches@.
--
--   Each automaton consists of
--
-- * one translation table (for symbol translation),
--
-- * transition tables (one for each state),
--
-- * table which maps states to their transition tables
--
-- * and table which for each state contains whether automaton matches.
generateFunctionCode :: GenFunc -> String
generateFunctionCode (RuN rn, n, genI)
  = case genI of
      GMatches re
        -> unlines [generateAutomaton (convertRegex re) "ms"
                   ,"bool matches_" ++ show rn ++ '_' : show n ++
                    paramList ["size_t start", "size_t len"] ++
                    "{ return genericMatches" ++
                    paramList ["start", "len"
                              ,tabName "ms" "translTab"
                              ,tabName "ms" "st2Match"
                              ,tabName "ms" "st2TransitTab"
                              ] ++
                    "; }"]
      GSplitWord re1 re2
        -> unlines [generateAutomaton (convertRegex re1) "sw1"
                   ,generateAutomaton (reverseRegex $ convertRegex re2) "sw2"
                   ,"void splitWord_" ++ show rn ++ '_' : show n ++
                     paramList ["size_t start", "size_t len"
                               ,"size_t *outStart1", "size_t *outLen1"
                               ,"size_t *outStart2", "size_t *outLen2"] ++
                    "{ genericSplitWord" ++
                    paramList ["start", "len"
                              ,"outStart1", "outLen1", "outStart2", "outLen2"
                              ,tabName "sw1" "translTab"
                              ,tabName "sw1" "st2Match"
                              ,tabName "sw1" "st2TransitTab"
                              ,tabName "sw2" "translTab"
                              ,tabName "sw2" "st2Match"
                              ,tabName "sw2" "st2TransitTab"
                              ] ++
                    "; }"]
  where
    paramList xs      = "(" ++ concat (intersperse ", " xs)  ++ ")"
    tabName name desc = "cap_" ++ show rn ++ '_' : show n ++ '_' : name ++
                        '_' : desc

    generateAutomaton :: Regex PartitionL Word8 Yes -> String -> String
    generateAutomaton regex name = unlines (translTab : transitTabs ++
                                            [st2Match, st2TransitTab])
      where
        translTab = buildTableU (camTranslationTabs matcher!0)
                      ("unsigned char " ++ tabName' "translTab") show

        transitTabs = map createTab $ assocs $ camTransitionTabs matcher
          where
            createTab (st, arr) = buildTableU arr
                                    ("StateNum " ++ tabName' "transitTab"
                                       ++ show st) show

        st2Match = buildTableL (elems $ camWhatMatches matcher)
                     ("bool " ++ tabName' "st2Match")
                     (\matches -> if null matches then "false" else "true")

        st2TransitTab = buildTableL (indices $ camTransitionTabs matcher)
                          ("StateNum * " ++ tabName' "st2TransitTab")
                          ((tabName' "transitTab" ++) . show)

        tabName' = tabName name

        matcher = toCompAlphabetMatcher 1 {- alphabet partitions -} dfa
        dfa     = kMinimize maxBound $ buildDFA [rule]
        rule    = Rule "" (Pr 1) Shortest regex (Subst [])

-- | Generates C code for capturing subwords of matched word
--   from abstract code.
generateCaptLangCode :: [CaptLang PartitionL Char]
                     -> State (RuNum, Int) String
generateCaptLangCode [] = return ""
generateCaptLangCode (IfNonEmpty w as:xs)
  = do c1 <- generateCaptLangCode as
       c2 <- generateCaptLangCode xs
       return $ unlines ["if(wLen" ++ show w ++ " != 0) {", c1, "}", c2]
generateCaptLangCode (IfMatches w _re as bs:xs)
  = do (RuN rule, lastId) <- get
       let newId = succ lastId
       put (RuN rule, newId)
       c1 <- generateCaptLangCode as
       c2 <- generateCaptLangCode bs
       c3 <- generateCaptLangCode xs
       return $ unlines ["if(matches_" ++ show rule ++ '_' : show newId ++
                         "(wStart" ++ show w ++ ", wLen" ++ show w ++ ")) {"
                        ,c1, "} else {", c2, "}", c3
                        ]
generateCaptLangCode (SplitWord w _re1 _re2 (wOut1, wOut2):xs)
  = do (RuN rule, lastId) <- get
       let newId = succ lastId
       put (RuN rule, newId)
       c <- generateCaptLangCode xs
       return $ unlines ["size_t wStart" ++ show wOut1 ++ ";"
                        ,"size_t wLen" ++ show wOut1 ++ ";"
                        ,"size_t wStart" ++ show wOut2 ++ ";"
                        ,"size_t wLen" ++ show wOut2 ++ ";"
                        ,"splitWord_" ++ show rule ++ '_' : show newId ++
                         "(wStart" ++ show w ++ ", wLen" ++ show w ++
                         ", &wStart" ++ show wOut1 ++
                         ", &wLen" ++ show wOut1 ++
                         ", &wStart" ++ show wOut2 ++
                         ", &wLen" ++ show wOut2 ++ ");"
                        ,c
                        ]
generateCaptLangCode (CaptureWord w i:xs)
  = do c <- generateCaptLangCode xs
       return $ unlines ["cStart" ++ show i ++ " = wStart" ++ show w ++ ";"
                        ,"cLen" ++ show i ++ " = wLen" ++ show w ++ ";"
                        ,c
                        ]

-- | Generates code which wites substitution into output stream called
--   @output@.
generateSubstCode :: Subst Char -> String
generateSubstCode subst = unlines $ map gen terms
  where
    gen (TCapture i) = "for(size_t i=cStart" ++ show i ++ "; i < cStart" ++
                       show i ++ " + cLen" ++ show i ++ "; ++i) " ++
                       "output.put(capVect[i]);"
    gen (TConst xs) = unlines $ map (\x -> "output.put(" ++ show x ++ ");") xs

    (Subst terms) = convertSubst subst

-- ---------------------------------------------------------------------------
-- Rule names

-- | Generates array with names of the rules.
generateRuleNames :: [Rule PartitionL Char] -> String
generateRuleNames rules = buildTableL (map toName rules)
                                      ("const char * rule2Name")
                                      show
  where
    toName (Rule name _ _ _ _) = name

-- ---------------------------------------------------------------------------
-- Partial order

-- | Generates function which setups initial partial order.
generatePOrder :: [Rule PartitionL Char] -> String
generatePOrder rules = unlines ["void setupPartialOrder(PartialOrder *po) {"
                               ,code
                               ,"}"
                               ]
  where
    -- Actual code which setups partial order.
    code = unlines $ map eachRule $ assocs $ fromRules rules

    eachRule (RuN rn, bitmask)
      = unlines $ map (\bit -> "po->ord[" ++ show rn ++
                               "][" ++ show bit ++ "] = true;")
                $ listBits bitmask 0

    listBits :: Integer -> Int -> [Int]
    listBits 0 _ = []
    listBits i bitNum
      | m /= 0    = bitNum : listBits i' (succ bitNum)
      | otherwise =          listBits i' (succ bitNum)
      where
        (i', m) = i `divMod` 2

-- ---------------------------------------------------------------------------
-- Static parts of the code

-- | Code.
beforeAutomaton :: String
beforeAutomaton  = [$verbatim|

// TODO: Before automaton

|]

-- | Code.
betweenAutomatonAndSubsts :: String
betweenAutomatonAndSubsts = [$verbatim|

// TODO: After automaton

|]

-- | Code.
afterSubsts :: String
afterSubsts = [$verbatim|

// TODO: After substitutions

|]

-- ---------------------------------------------------------------------------
-- Helper functions

-- | Returns list with captures from substitution.
capturesInSubst :: Subst a -> [Int]
capturesInSubst (Subst terms)
  = catMaybes $ map (\t -> case t of TCapture i -> Just i
                                     TConst _   -> Nothing) terms

-- | C array from boxed array. Second contains type singature and name
--   of the variable.
buildTable :: Array Int e -> String -> (e -> String) -> String
buildTable arr = buildTableL (elems arr)

-- | C array from unboxed array.
buildTableU :: (Show i, Enum i, Ix i, U.IArray UArray e)
            => UArray i e -> String -> (e -> String) -> String
buildTableU arr = buildTableL (U.elems arr)

-- | C array from list.
buildTableL :: [e] -> String -> (e -> String) -> String
buildTableL list typeAndName elemToStr
  = typeAndName ++ '[' : show (length list) ++ "] = "
    ++ '{' : (concat $ intersperse "," $ map elemToStr list) ++ "};"
