import Test.HUnit

import Core.Partition
import Core.Matcher
import Core.Regex
import Core.Rule
import Data.Array
import Core.DFA (buildDFA, kMinimize, updateReachablePrio, updateWhatMatches)
import Core.UTF8

main :: IO ()
main = do runTestTT suite
          return ()

toCAM :: (Enum s, Bounded s, Ix s)
      => [Rule PartitionL s] -> CompAlphabetMatcher s
toCAM rules = toCompAlphabetMatcher 1 dfa
    where
      dfa = kMinimize maxBound $ updateReachablePrio
                               $ updateWhatMatches rules
                               $ buildDFA rules

regexToRule :: Regex PartitionL s Yes -> Rule PartitionL s
regexToRule re = Rule "re" (Pr 1) Shortest re (Subst [])

toCAMRegex :: (Enum s, Bounded s, Ix s)
           => Regex PartitionL s Yes -> CompAlphabetMatcher s
toCAMRegex = toCAM . (\a -> [a]) . regexToRule

testCAMRegex :: Regex PartitionL Char Yes -> String -> [Length] -> Test
testCAMRegex re w expected
  = (findWords m w, findWords m' w') ~?=
    ([(RuN 0, expected)], [(RuN 0, expected')])
  where
    m  = toCAMRegex re
    m' = toCAMRegex $ convertRegex re
    w' = convertString w
    expected' = let  is = scanl (+) 0 $ map (length . convertChar) w
                in map (is!!) expected

t_And = "And" ~: test
  [
    let re = And (CharClass $ fromRanges [Range 'a' 'a'])
                 (Repeat 1 20 $ CharClass alphabet)
    in testCAMRegex re "ab" [1]
  ]

t_Not = "Not" ~: test
  [
    let re = Not $ CharClass $ fromRanges [Range 'a' 'a']
    in testCAMRegex re "xya" [1, 2, 3]
  , let re = Not $ Concat (CharClass $ fromRanges [Range 'a' 'a'])
                          (CharClass $ fromRanges [Range 'n' 'n'])
    in testCAMRegex re "ano" [1, 3]
  , let a = CharClass $ fromRanges [Range 'a' 'a']
        dotStar = RepeatU 0 (CharClass alphabet)
        re = -- ^(? .* a .* )
             Not $ Concat dotStar (Concat a dotStar)
    in testCAMRegex re "pes ahoj" [1..4]
  , let ab      = Concat (CharClass $ fromRanges [Range 'a' 'a'])
                         (CharClass $ fromRanges [Range 'b' 'b'])
        dotStar = RepeatU 0 (CharClass alphabet)
        re = -- ^(? .* ab .* ) ab
             Concat (Not $ Concat dotStar (Concat ab dotStar))
                    ab
    in testCAMRegex re "ahoj ab zdar ab" [7]
  ]

suite = test
  [
    t_And, t_Not
  ]
