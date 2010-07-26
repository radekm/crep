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
module BackEnd.CPP
       (
         generateCode
       ) where

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
                               in if i /= 0
                                    then "size_t cStart" ++ s ++ "=0;" ++
                                         "size_t cLen"   ++ s ++ "=0;"
                                    else "size_t cStart0=wStart0;" ++
                                         "size_t cLen0=wLen0;") $
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
        definedCaptures  = 0:listCaptures regex

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
                       "output_subst(output, capVect[i]);"
    gen (TConst xs) = unlines $
                      map (\x -> "output_subst(output, " ++ show x ++ ");") xs

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

// Flags:
// #define SELECT_FIRST
// #define NO_CONTEXT

#include <bitset>
#include <iostream>
#include <list>
#include <vector>
#include <fstream>
#include <cassert>
#include <climits>

using namespace std;

typedef size_t RuNum;
typedef size_t Length;
typedef size_t StateNum;

#ifndef SELECT_FIRST

#ifdef NO_CONTEXT
#define SHOW_PREV_CHARS 0
#else
#define SHOW_PREV_CHARS 30
#endif

// Pocet zpracovanych pismen, ktere si system uchovava. Tato jsou pouzita
#define CNT_MEMO_CHARS 32
unsigned char memoChars[CNT_MEMO_CHARS];
Length nextMemoChar = 0; // Kam prijde dalsi znak k uchovani.
                         // Kdyz je nMemoChars == CNT_MEMO_CHARS, tak je to
                         // take index nejstarsiho znaku.
Length nMemoChars = 0; // Kolik znaku skutecne uchovavame.
#endif

|]

-- | Code.
betweenAutomatonAndSubsts :: String
betweenAutomatonAndSubsts = [$verbatim|

// ---------------------------------------------------------------------------
// Partial order of the rules

class PartialOrder {
public:
    bitset<CNT_RULES> ord[CNT_RULES];

    PartialOrder() {
        for(int rule = 0; rule < CNT_RULES; ++rule) {
            ord[rule].reset();
        }
    }
    bool isHigher(const RuNum lo, const RuNum hi) const {
        return ord[lo][hi]; // If the bit is set then hi is higher rule.
    }
    void mkHigher(RuNum lo, RuNum hi) {
        ord[lo][hi] = true; // Rule "hi" is higher than "lo".
        ord[lo] |= ord[hi]; // Rules higher than "hi" are higher than "lo".

        for(int rule = 0; rule < CNT_RULES; ++rule) {
            // Rule "rule" is lower than "lo".
            if(ord[rule][lo]) {
                // Rules higher than "lo" are higher than "rule"
                // (transitivity).
                ord[rule] |= ord[lo];
            }
        }
    }
    bitset<CNT_RULES> getHigher(RuNum rule) const {
        return ord[rule]; // Rules higher than "rule":.
    }

    void show() {
        for(size_t rule = 0; rule < CNT_RULES; ++rule) {
            cout << "higher than rule " << rule2Name[rule] << ":";
            bool noHigher = true;

            for(size_t r = 0; r < CNT_RULES; ++r) {
                // r je vyssi nez rule
                if(!isHigher(rule, r))
                    continue;

                // neexistuje nic vyssiho nez rule, co je nizsi nez r
                size_t rr = 0;
                for(; rr < CNT_RULES; ++rr) {
                    // r nepokryva rule, protoze je mezi nima rr
                    if(isHigher(rule, rr) && isHigher(rr, r))
                        break;
                }

                // r pokryva rule
                if(rr == CNT_RULES) {
                    if(noHigher) {
                        cout << " ";
                        noHigher = false;
                    }
                    else
                        cout << ", ";
                    cout << rule2Name[r];
                }
            }

            // Each rule has its line.
            cout << endl;
        }
    }
};

void separator() {
    cout << "---------------------------------------------------------------";
    cout << "---------" << endl;
}

#ifndef SELECT_FIRST
// zapamatuje si znak
void memo_char(unsigned char c) {
    memoChars[nextMemoChar] = c;
    ++nextMemoChar;
    if(nMemoChars == CNT_MEMO_CHARS)
        nextMemoChar %= CNT_MEMO_CHARS;
    else /* nMemoChars < CNT_MEMO_CHARS */
        ++nMemoChars;
}

bool memoizeSubstOutput = false;
#endif

// vystup pro substituci; lze nastavit, jestli znaky maji byt uchovany
void output_subst(ostream &output, unsigned char c) {
#ifndef SELECT_FIRST
    if(memoizeSubstOutput)
        memo_char(c);
#endif
    output.put(c);
}

// Vektor obsahuje znaky slova, co bylo namatchovano (delka az K*4).
// Pouzivaji ho funkce genericMatches, genericSplitWord, vygenerovane funkce
// subst_i (kde i je cislo pravidla). Vektor lze naplnit metodou
// putIntoCapVect.
vector<unsigned char> capVect;

// Pouzito funkci genericSplitWord (detaily pospany tam).
bitset<K*4+1> capBools;

bool genericMatches(size_t start, size_t len,
        unsigned char *translTab, bool *st2Matches, StateNum **transitTab) {

    StateNum state = 0;
    for(size_t i = start; i < start + len; ++i) {
        int translatedSymb = translTab[capVect[i]];
        state = transitTab[state][translatedSymb];
    }

    return st2Matches[state];
}

void genericSplitWord(size_t start, size_t len,
        size_t *outStart1, size_t *outLen1,
        size_t *outStart2, size_t *outLen2,
        unsigned char *translTab1, bool *st2Matches1, StateNum **transitTab1,
        unsigned char *translTab2, bool *st2Matches2, StateNum **transitTab2){

    // capBools[i] = whether first automaton matched after it read i
    //characters
    capBools.reset();

    // first automaton
    StateNum state = 0;
    size_t nProcChars = 0;
    capBools[nProcChars] = st2Matches1[state];
    while(nProcChars < len) {
        int translatedSymb = translTab1[capVect[nProcChars]];
        state = transitTab1[state][translatedSymb];
        capBools[++nProcChars] = st2Matches1[state];
    }

    // second automaton
    state = 0;
    if(capBools[len] && st2Matches2[state]) {
        *outStart1 = start;
        *outLen1 = len;
        *outStart2 = start + len;
        *outLen2 = 0;
        return;
    }
    nProcChars = 0;
    while(nProcChars < len) {
        ++nProcChars;
        int translatedSymb = translTab2[capVect[start + len - nProcChars]];
        state = transitTab2[state][translatedSymb];

        if(capBools[len - nProcChars] && st2Matches2[state]) {
            *outStart1 = start;
            *outLen1 = len - nProcChars;
            *outStart2 = start + len - nProcChars;
            *outLen2 = nProcChars;
            return;
        }
    }

    assert(0);
}

|]

-- | Code.
afterSubsts :: String
afterSubsts = [$verbatim|

class Character;
class Alternative;

ifstream input;
ofstream output;
Character *text;

// na zacatku jsou vsechny alternativy v original
// 0. pismena, kde nezacina zadne slovo preskocim (vypisi na vystup)
// 1. zkopiruji original do toDecide
// 2. rozhodnu vsechny alternativy;
// 3. zkonstruuji nahradu pro kazde z vybranych slov
// 4. pokud je mozne vybrat alespon 2 ruzna slova, pak je musi vybrat uzivatel;
//    uzivatel muze take rict, ze chce zachovat puvodni usporadani
// 5. pokud neni mozne vybrat vice ruznych slov, tak vyberu to jedine
//    automaticky a necham si usporadani, co mam
// 6. az je slovo vybrano, zapisu pismena pred a nahradu na vystup;
//    z textu odstranim vsechna pismena az po posledni pismeno vybraneho slova
//    (posledni take odstranim, ale to za nim uz ne)
// 7. pokud si uzivatel nevybral, ze chce ponechat puvodni usporadani,
//    pak orig uvolnim, z decided odstranim usporadani, co nemaji vybrane slovo
//    a zbyla usporadani presunu do original (musim odebrat zpracovana pismena)
// 8. pokud uzivatel chce ponechat puvodni usporadani: odstranim decided,
//    z originalnich alternativ si necham jen usporadani (tedy znaky
//    a s nimi i vybrana slova odstranim)
// 9. toto opakuji, dokud nedojdu na konec textu
//
// Ve vsech usporadanich dojdou slova najednou.
Alternative *original;
Alternative *toDecide;
Alternative *decided;

// Kolikrat bylo na vyber vic nez jedno slovo.
size_t numOfConflicts;

// ---------------------------------------------------------------------------
// Words which start at fixed position and match fixed rule.

class Words {
public:
    RuNum rule;
    list<Length> lens; // Can be empty.

    Words(RuNum rule, Length len) {
        this->rule = rule;
        lens.push_front(len);
    }
    void addLength(Length len) {
        if(!shortest[rule]) // Only insert when rule is non-shortest.
            lens.push_front(len);
    }
};

void vectorWhatMatches(const list<Words> &ws, bitset<CNT_RULES> &whatMatches) {
    list<Words>::const_iterator it = ws.begin();
    while(it != ws.end()) {
        if(!it->lens.empty()) {
            whatMatches[it->rule] = true;
        }
        ++it;
    }
}

bool isMaximal(const PartialOrder &po, const bitset<CNT_RULES> &whatMatches,
        RuNum rule) {

    bitset<CNT_RULES> cmp = po.getHigher(rule) & whatMatches;
    return cmp.none();
}

void selectMaxWord(const list<Words> &ws, const PartialOrder &po,
        RuNum *rule, Length *len, bool *selected) {

    bitset<CNT_RULES> whatMatches;
    vectorWhatMatches(ws, whatMatches);

    // Select first rule which is not diff and is not smaller than any other
    // rule which match here.
    list<Words>::const_iterator it = ws.begin();
    while(it != ws.end()) {
        if(!it->lens.empty()) {
            if(isMaximal(po, whatMatches, it->rule)) {
                *selected = true;
                *rule = it->rule;
                *len = it->lens.front();
                return;
            }
        }
        ++it;
    }

    *selected = false;
}

void selectMaxWordAndHigherThan(const list<Words> &ws, const PartialOrder &po,
        const RuNum lower, RuNum *rule, Length *len, bool *selected) {

    bitset<CNT_RULES> whatMatches;
    vectorWhatMatches(ws, whatMatches);

    // Select first rule which is not diff and is not smaller than any other
    // rule which match here.
    list<Words>::const_iterator it = ws.begin();
    while(it != ws.end()) {
        if(!it->lens.empty()) {
            if(po.isHigher(lower, it->rule)
                    && isMaximal(po, whatMatches, it->rule)) {

                *selected = true;
                *rule = it->rule;
                *len = it->lens.front();
                return;
            }
        }
        ++it;
    }

    *selected = false;
}

// ---------------------------------------------------------------------------
// Automaton for searching for words

// First byte of code point in UTF-8 encoding.
bool firstUTF8Byte(unsigned char c) {
    return (c < 128 || (c&224) == 192 || (c&240) == 224 || (c&248) == 240);
}

class Automaton {
public:
    StateNum state;
    Length realLen;
    Length unicodeLen;
    int maxMatchedPrio;

    Automaton() {
        state = 0;
        realLen = 0;
        unicodeLen = 0;
        maxMatchedPrio = -1;
    }
    void next(unsigned char c, list<Words> &toUpdate) {
        realLen++;

        // First byte of UTF-8 sequence.
        if(firstUTF8Byte(c))
            ++unicodeLen;

        unsigned char translated = mSt2TranslTab[state][c];
        state = mSt2TransitTab[state][translated];

        // Update what matches.
        for(size_t i = 0; i < mSt2WhatMatchesLength[state]; ++i) {
            RuNum rule = mSt2WhatMatchesArr[state][i];

            list<Words>::iterator it = toUpdate.begin();
            while(it != toUpdate.end()) {
                if(it->rule == rule)
                    break;
                ++it;
            }
            // Rule wasn't found in the list, we add it.
            if(it == toUpdate.end())
                toUpdate.push_front(Words(rule, realLen));
            // Rule was found, we add only new length.
            else
                it->addLength(realLen);

            if(priorities[rule] > maxMatchedPrio)
                maxMatchedPrio = priorities[rule];
        }

        // upravit to nejvyssi, co matchovalo
    }
    bool shouldReadNextCharacter() {
        if(realLen >= 4*K || unicodeLen >= K
                || maxMatchedPrio > mSt2ReachablePrio[state])

            return false;

        return true;
    }
};

// ---------------------------------------------------------------------------
//

enum CharacterState {
    S_UNKNOWN,
    S_READ,
    S_WORDS_FOUND,
    S_EOF
};

class Character {
public:
    // S_READ - content was set but variable words is empty
    // WORDS_FOUND - content was set and words are filled
    // S_UNKNOWN, S_EOF - next is NULL; content and words are not set
    CharacterState state;
    unsigned char content;
    list<Words> words;
    Character *next;

    Character() {
        state = S_UNKNOWN;
        next = NULL;
    }
    // The state after this is different from S_UNKNOWN.
    int read() {
        int c;
        switch(state) {
            case S_EOF:
                return -1;
            case S_UNKNOWN:
                c = input.get();

                if(!input.good()) {
                    state = S_EOF;
                    return -1;
                }

                content = c;
                state = S_READ;
                next = new Character;
            default:
                return content;
        }
    }
    // The state after this is S_WORDS_FOUND or S_EOF.
    void findWords() {
        read(); // ensure that current state is not S_UNKNOWN

        if(state == S_WORDS_FOUND || state == S_EOF)
            return;

        // current state must be S_READ
        state = S_WORDS_FOUND;

        Character *toRead = this;
        int c;
        Automaton dfa;

        while(dfa.shouldReadNextCharacter()) {
            c = toRead->read();
            if(c == -1) // No next character available.
                return;

            dfa.next(c, words);
            toRead = toRead->next;
        }
    }
};

class CharacterAlt {
public:
    // If true then words are empty and parent has any state and next is NULL.
    // If false then parent has state S_EOF, words are empty and next is NULL
    // or parent has state S_WORDS_FOUND and variable words contains words
    // from parent and next is not NULL.
    bool unknown; // Only last node can be unknown.
    Character *parent;
    list<Words> words;
    CharacterAlt *next;
    RuNum selRule;
    Length selLength; // Zero when nothing selected.

    CharacterAlt(Character *par) {
        unknown = true;
        parent = par;
        next = NULL;
        selLength = 0;
    }
    CharacterAlt(const CharacterAlt &a) {
        unknown = a.unknown;
        parent = a.parent;
        words = a.words;
        next = NULL;
        selRule = a.selRule;
        selLength = a.selLength;
    }
    // After this unknown = false and state of the parent is S_WORDS_FOUND
    // or S_EOF.
    void read() {
        if(!unknown)
            return;

        unknown = false;

        parent->findWords();

        if(parent->state == S_WORDS_FOUND) {
            words = parent->words;
            next = new CharacterAlt(parent->next);
        }
        // else parent->state == S_EOF
    }
};

class Alternative {
public:
    PartialOrder ord;
    CharacterAlt *chars;

    Alternative *next;
    Alternative(Character *parentCharacter) {
        chars = new CharacterAlt(parentCharacter);
        next = NULL;
    }
    Alternative(const Alternative &a) {
        ord = a.ord;
        chars = NULL;
        next = NULL;

        CharacterAlt **toModify = &chars; // where the duplicate will be stored
        CharacterAlt *toDup = a.chars;

        while(toDup) {
            *toModify = new CharacterAlt(*toDup);
            toModify = &((*toModify)->next);
            toDup = toDup->next;
        }
    }
    ~Alternative() {
        // Remove characters.
        while(chars != NULL) {
            CharacterAlt *toDelete = chars;
            chars = chars->next;
            delete toDelete;
        }
    }
    void selectWord() {
        CharacterAlt *curWord;
        CharacterAlt *c = chars;
        bool selected = false;
        RuNum r;
        Length l;
        size_t fromStart = 0;

        // Najdeme prvni slovo (to by se melo najit hned na prvni pozici).
        while(!selected) {
            c->read();
            curWord = c;
            selectMaxWord(curWord->words, ord, &r, &l, &selected);

            c = c->next;
            ++fromStart;
        }

        size_t toCheck = l - 1;
        // Nyni je prvni slovo nalezeno. Najdeme slovo, co se neprekryva
        // s vyssim.
        while(toCheck > 0) {
            c->read();
            selectMaxWordAndHigherThan(c->words, ord, r, &r, &l, &selected);

            // Higher word was selected.
            if(selected) {
                curWord = c;
                toCheck = l;
            }

            --toCheck;
            c = c->next;
            ++fromStart;
        }
        // Nyni c ukazuje za posledni znak toho slova a fromStart obsahuje
        // pocet znaku, ktere je treba proverit (resp. pokud znaky indexujeme
        // od nuly, tak fromStart je index prvniho znaku za vybranym slovem).
        //
        // Tedy slovo zacina: fromStart - l
        size_t wordStart = fromStart - l;

        // Udelame slovo vyssi nez vse, co se s nim prekryva.
        c = chars;
        for(size_t n = 0; n < fromStart; ++n, c = c->next) {
            list<Words>::const_iterator it = c->words.begin();
            while(it != c->words.end()) {
                // Slovo neni ostre nizsi (a neni to, co mam vybrane).
                if(r != it->rule && !it->lens.empty()
                        && !ord.isHigher(it->rule, r)) {

                    Length longest = it->lens.front();

                    // Slovo se prekryva s vybranym (konci za jeho zacatkem).
                    if(n + longest > wordStart) {
                        Alternative *dup = new Alternative(*this);
                        dup->ord.mkHigher(r, it->rule);
                        dup->next = toDecide;
                        toDecide = dup;

                        ord.mkHigher(it->rule, r);
                    }
                }
                ++it;
            }
        }

        // Nyni odstranime slova, co se stim mym prekryvaji.
        // Pred ostranim jen slova, co do meho zasahuji.
        // Uvnitr odstranim vsechna slova.

        // Slova co zacinaji drive a prekryvaji se.
        c = chars;
        size_t n = 0;
        Length maxLength = wordStart; // Nejvyssi delka, co se neprekryva.
        for(; n < wordStart; ++n, --maxLength, c = c->next) {
            list<Words>::iterator it = c->words.begin();
            while(it != c->words.end()) {
                list<Length>::iterator lit = it->lens.begin();
                // Odstranime delky, co jsou ostre vyssi nez maxLength.
                while(lit != it->lens.end() && *lit > maxLength) {
                    list<Length>::iterator toDelete = lit;
                    ++lit;
                    it->lens.erase(toDelete);
                }
                ++it;
            }
        }
        // Nastavim vybrane slovo.
        c->selRule = r;
        c->selLength = l;
        // Slova, ktera zacinaji uvnitr vybraneho.
        for(; n < fromStart; ++n, c = c->next) {
            c->words.clear();
        }
    }
    bool isFirstWordSelected() {
        CharacterAlt *c = chars;
        while(c->selLength == 0) {
            c->read();

            // If there are any words return false.
            list<Words>::const_iterator it = c->words.begin();
            while(it != c->words.end()) {
                if(!it->lens.empty())
                    return false;
                ++it;
            }

            c = c->next;
        }

        return true;
    }
    void decide() {
        while(!isFirstWordSelected())
            selectWord();

        this->next = decided;
        decided = this;
    }
};

class SelWord {
public:
    Character *c;
    Length nCharsBefore;
    RuNum rule;
    Length len;

    SelWord(Character *c, RuNum r, Length l, Length nCharsBefore) {
        this->c = c;
        this->rule = r;
        this->len = l;
        this->nCharsBefore = nCharsBefore;
    }
    void putIntoCapVect() const {
        capVect.clear();
        Character *cur = c;
        for(size_t i = 0; i < len; ++i, cur = cur->next) {
            capVect.push_back(cur->content);
        }
    }
};

void removeCharFromText() {
    Character *toDelete = text;
    text = text->next;
    delete toDelete;
}

bool processWord() {
    // After this there will start word at first character.
    while(true) {
        text->findWords();
        if(text->state == S_EOF)
            return false;
        if(text->words.empty()) {
            output.put(text->content);

#ifndef SELECT_FIRST
            // Memoize character.
            memo_char(text->content);
#endif

            // Remove character from the text.
            Character *toSkip = text;
            text = toSkip->next;
            delete toSkip;

            // Remove character from each alternative.
            Alternative *a = original;
            while(a) {
                // Last character in the alternative.
                if(a->chars->unknown)
                    a->chars->parent = text;
                else {
                    CharacterAlt *toDelete = a->chars;
                    a->chars = a->chars->next;
                    delete toDelete;
                }
                a = a->next;
            }
        }
        else {
            break;
        }
    }

    Alternative *a;
#ifndef SELECT_FIRST
    // Copy alternatives to toDecide.
    a = original;
    Alternative **dest = &toDecide;
    while(a) {
        *dest = new Alternative(*a);
        a = a->next;
        dest = &((*dest)->next);
    }
#else
    // Move alternatives to toDecide;
    toDecide = original;
    original = NULL;
#endif

    // Decide each alternative.
    while(toDecide) {
        a = toDecide;
        toDecide = toDecide->next;
        a->decide();
    }

    // Collect words which can be selected into a list (each word once).
    a = decided;
    list<SelWord> selWords;
    while(a) {
        CharacterAlt *charAlt = a->chars;

        // Find character where selected word starts.
        Length nCharsBefore = 0;
        while(charAlt->selLength == 0) {
            charAlt = charAlt->next;
            ++nCharsBefore;
        }

        // Insert selected word if not already there.
        list<SelWord>::const_iterator wit = selWords.begin();
        while(wit != selWords.end()) {
            // Word found.
            if(wit->c == charAlt->parent && wit->len == charAlt->selLength
                    && wit->rule == charAlt->selRule)
                break;
            ++wit;
        }
        // Word not found, add it.
        if(wit == selWords.end()) {
            selWords.push_back(SelWord(charAlt->parent, charAlt->selRule,
                    charAlt->selLength, nCharsBefore));
        }
        a = a->next;
    }


    bool originalAlternatives = false;
    int selectedNum = 1; // musi byt <= size a >= 1 -- cislovano od 1
    list<SelWord>::const_iterator sit;

#ifndef SELECT_FIRST
    // Vice nez jedno slovo, nechame uzivatele vybrat.
    if(selWords.size() > 1) {
        separator();
        cout << "Select 1 of " << selWords.size() << " words:" << endl << endl;

        // Vypiseme vsechny moznosti.
        sit = selWords.begin();
        int wordNumber = 1;
        while(sit != selWords.end()) {
            sit->putIntoCapVect();
            cout << wordNumber << ". rule: " << rule2Name[sit->rule] << endl;
            cout << "    old: ";
            for(size_t i = 0; i < sit->len; ++i)
                cout << capVect[i];
            cout << endl;
            cout << "    new: ";
            computeSubst(sit->rule, cout);
            cout << endl;
            cout << "    ctx: ";

            // ukazat SHOW_PREV_CHARS znaku pred nahradou
            size_t fromMemoChars = 0; // kolik znaku potrebuji z memoChars
            if(sit->nCharsBefore < SHOW_PREV_CHARS)
                fromMemoChars = SHOW_PREV_CHARS - sit->nCharsBefore;
            if(fromMemoChars > nMemoChars)
                fromMemoChars = nMemoChars;

            // priznak zda-li byl uz nalezen zacatek UTF8 sekvence
            bool utf8Start = false;

            // vypisi znaky z memoChars
            size_t written = 0;
            size_t memoIdx = nextMemoChar + CNT_MEMO_CHARS - fromMemoChars;
            for(; written < fromMemoChars; ++written, ++memoIdx) {
                unsigned char toWrite = memoChars[memoIdx % CNT_MEMO_CHARS];
                if(!utf8Start)
                    utf8Start = firstUTF8Byte(toWrite);
                // Vypisujeme pouze pokud byl nalezen zacatek UTF8 sekvence -
                // pak vypiseme uplne vsechny znaky.
                if(utf8Start)
                    cout << toWrite;
            }

            // kolik znaku pred vybranym slovem preskocim
            size_t skipBeforeSelWord = 0;
            if(sit->nCharsBefore > SHOW_PREV_CHARS)
                skipBeforeSelWord = sit->nCharsBefore - SHOW_PREV_CHARS;

            // preskocim znaky pred
            Character *c = text;
            written = 0;
            for(; written < skipBeforeSelWord; ++written, c = c->next) ;

            // vypisu znaky pred
            for(; written < sit->nCharsBefore; ++written, c = c->next) {
                unsigned char toWrite = c->content;
                if(!utf8Start)
                    utf8Start = firstUTF8Byte(toWrite);
                // Vypisujeme pouze pokud byl nalezen zacatek UTF8 sekvence -
                // pak vypiseme uplne vsechny znaky.
                if(utf8Start)
                    cout << toWrite;
            }

            computeSubst(sit->rule, cout);
            cout << endl << endl;

            ++sit;
            ++wordNumber;
        }

        // Nechame uzivatele vybrat.
        while(true) {
            cout << "select word: ";
            cin >> selectedNum;

            if(cin.eof())
                return false;

            if(cin.good()) {
                if(selectedNum < 0) {
                    originalAlternatives = true;
                    selectedNum = -selectedNum;
                }

                // There is no such alternative.
                if(selectedNum == 0 || selectedNum > (int)selWords.size())
                    continue;

                // OK.
                break;
            }
            else {
                cin.clear();
                std::cin.ignore(INT_MAX,'\n');
            }
        }
    }
#endif

    // Zapocitej konflikt.
    numOfConflicts += (selWords.size() > 1);

    sit = selWords.begin();
    for(int i = 1; i < selectedNum; ++i, ++sit) ;
    SelWord selectedWord(*sit);

    {
        // Write characters before selected word.
        for(size_t nRemove = selectedWord.nCharsBefore; nRemove > 0; --nRemove) {
            output.put(text->content);
#ifndef SELECT_FIRST
            memo_char(text->content);
#endif
            removeCharFromText();
        }

        // Write substitution.
#ifndef SELECT_FIRST
        memoizeSubstOutput = true;
#endif
        selectedWord.putIntoCapVect();
        computeSubst(selectedWord.rule, output);
#ifndef SELECT_FIRST
        memoizeSubstOutput = false;
#endif

        // Skip selected word.
        for(size_t nRemove = selectedWord.len; nRemove > 0; --nRemove)
            removeCharFromText();
    }

    // User wants new alternatives.
    if(!originalAlternatives) {
#ifndef SELECT_FIRST
        // Remove alternatives from original.
        while(original) {
            a = original;
            original = original->next;
            delete a;
        }
#endif
        // Remove alternatives with different word, remaining alternatives
        // are moved to original.
        while(decided) {
            CharacterAlt *charAlt = decided->chars;

            while(charAlt->selLength == 0)
                charAlt = charAlt->next;

            // Alternative contains selected word.
            if(charAlt->selLength == selectedWord.len
                    && charAlt->selRule == selectedWord.rule
                    && charAlt->parent == selectedWord.c) {

                a = decided->next;
                decided->next = original;
                original = decided;
                decided = a;
            }
            // Alternative does not contain selected word.
            else {
                a = decided;
                decided = decided->next;
                delete a;
            }
        }
        // Remove processed characters from remaining alternatives.
        a = original;
        while(a) {
            size_t nRemove = selectedWord.nCharsBefore + selectedWord.len;

            while(nRemove > 0) {
                CharacterAlt *toDelete = a->chars;
                a->chars = a->chars->next;
                delete toDelete;
                --nRemove;
            }
            a = a->next;
        }
    }
#ifndef SELECT_FIRST
    // User wants original alternatives.
    else {
        // Free new alternatives.
        while(decided) {
            Alternative *toDelete = decided;
            decided = decided->next;
            delete toDelete;
        }

        // Free all characters from original alternatives.
        a = original;
        while(a) {
            while(a->chars) {
                CharacterAlt *toDelete = a->chars;
                a->chars = a->chars->next;
                delete toDelete;
            }
            a->chars = new CharacterAlt(text);
            a = a->next;
        }
    }
#endif

    return true;
}

void processText() {
    while(processWord()) ;
}

int main(int argc, char** argv) {
    // otevreni vstupu
    if(argc != 3) {
        cout << "Usage: " << argv[0] << " input-file output-file" << endl;
        return 0;
    }
    input.open(argv[1]);
    if(!input.is_open()) {
        cout << "Input file cannot be opened." << endl;
        return 0;
    }
    output.open(argv[2]);
    if(!output.is_open()) {
        input.close();
        cout << "Output file cannot be opened." << endl;
        return 0;
    }

    // inicializace
    text = new Character();
    original = new Alternative(text);
    setupPartialOrder(&original->ord);

    processText();

    // vypisu usporadani
    separator();
    cout << "Number of conflicts: " << numOfConflicts << endl;
    cout << "Alternatives: " << endl;

    // uvolnim alternativy v original
    while(original) {
        Alternative *toDelete = original;
        original = original->next;

        // vypisu usporadani
        cout << endl;
        toDelete->ord.show();
        cout << endl;

        delete toDelete;
    }
    // uvolnim posledni znak eof
    delete text;

    output.close();
    input.close();
    return 0;
}

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
