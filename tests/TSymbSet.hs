
import Test.HUnit

import Core.SymbSet
import Core.Partition.Internal
import Data.Bits ((.|.))

main :: IO ()
main = do runTestTT suite
          return ()

-- udelat testy na bajty (staci jich min)

t_mkRange = "mkRange" ~: test
    [ mk 'a' 'a' ~=? mk 'a' 'a'
    , mk 'a' 'b' ~=? mk 'a' 'b'
    , False ~=? (mk 'a' 'a' == mk 'A' 'A')
    , False ~=? (mk 'a' 'b' == mk 'a' 'c')
    , False ~=? (mk 'a' 'a' == mk 'b' 'b')
    ]
  where
    mk = mkRange

t_loR = "loR" ~: test
    [ 'a' ~=? loR (mk 'a' 'a')
    , 'a' ~=? loR (mk 'a' 'c')
    , 'A' ~=? loR (mk 'A' 'a')
    , loR (mk 'a' 'a') ~=? loR (mk 'a' 'b')
    , loR (mk 'X' 'z') ~=? loR (mk 'X' 'Y')
    ]
  where
    mk = mkRange

t_hiR = "hiR" ~: test
    [ 'a' ~=? hiR (mk 'a' 'a')
    , 'D' ~=? hiR (mk 'B' 'D')
    , hiR (mk 'f' 'h') ~=? hiR (mk '0' 'h')
    , loR (mk 'f' 'i') ~=? hiR (mk '\0' 'f')
    ]
  where
    mk = mkRange

t_alphabet_empty = "alphabet, empty" ~: test
    [ False ~=? (alphabet == (empty :: CharSet))
    , alphabet ~=? fromRanges [mkRange minBound (maxBound :: Char)]
    ]

t_fromRanges = "fromRanges" ~: test
    [ fr [mk 'a' 'a'] ~=? fr [mk 'a' 'a']
    , fr [mk 'a' 'a'] ~=? fr [mk 'a' 'a', mk 'a' 'a']
    , fr [mk 'a' 'b'] ~=? fr [mk 'a' 'a', mk 'b' 'b']
    , fr [mk 'a' 'd'] ~=? fr [mk 'a' 'b', mk 'c' 'd']
    , fr [mk 'a' 'e'] ~=? fr [mk 'a' 'c', mk 'b' 'e']
    , fr [mk 'a' 'e'] ~=? fr [mk 'a' 'b', mk 'b' 'b', mk 'd' 'e', mk 'c' 'c']
    , fr [mk 'c' 'd', mk 'a' 'a'] ~=? fr [mk 'a' 'a', mk 'd' 'd', mk 'c' 'c']
    , fr [mk 'a' 'g', mk 'A' 'a', mk 'c' 'd', mk 'i' 'l', mk 'm' 'z'] ~=?
      fr [mk 'A' 'g', mk 'i' 'z']
    ]
  where
    fr = fromRanges
    mk = mkRange

t_toRanges = "toRanges" ~: test
    [ [mk 'a' 'a'] ~=? to (fr [mk 'a' 'a'])
    , [mk 'a' 'b'] ~=? to (fr [mk 'a' 'b'])
    , [mk 'a' 'e'] ~=?
      to (fr [mk 'a' 'a', mk 'd' 'd', mk 'e' 'e', mk 'c' 'c', mk 'b' 'b'])
    , [mk 'F' 'M', mk 'c' 'd'] ~=? to (fr [mk 'c' 'd', mk 'F' 'M'])
    , let x = fr [mk 'g' 'j', mk '0' '8', mk 'a' 'b', mk 'c' 'd', mk 'X' 'X']
      in x ~=? (fr $ to x)
    ]
  where
    to = toRanges
    fr = fromRanges
    mk = mkRange

t_toPartition = "toPartition" ~:
    [ PC (toValueC maxBound) NilC ~=? toPartition empty
    ]

t_fromPartition = "fromPartition" ~:
    [ let x = fromRanges [mkRange minBound 'a']
      in mt [x, complement x] ~=?
         mt (fr (PC (toValueC 'a')
                    (PC (oneC .|. toValueC maxBound) NilC)))
    ]
  where
    fr = fromPartition
    to = toPartition
    mt = map to

t_member = "member" ~: test
    [ True ~=? mem 'a' (fr [mk 'a' 'a'])
    , True ~=? mem 'c' (fr [mk 'a' 'z'])
    , True ~=? mem 'c' (fr [mk 'A' 'B', mk 'C' 'E', mk 'a' 'c'])
    , True ~=? mem 'c' (fr [mk 'A' 'F', mk 'c' 'e'])
    , False ~=? mem 'c' (fr [mk 'A' 'F', mk 'a' 'b'])
    , False ~=? mem 'c' (fr [mk 'A' 'F', mk 'a' 'b', mk 'd' 'z'])
    ]
  where
    fr  = fromRanges
    mk  = mkRange
    mem = member

t_firstSymb = "firstSymb" ~: test
    [ 'a' ~=? firstSymb (fr [mk 'a' 'd'])
    , 'A' ~=? firstSymb (fr [mk 'a' 'd', mk 'A' 'b'])
    , '\0' ~=? firstSymb alphabet
    ]
  where
    mk = mkRange
    fr = fromRanges

t_complement = "complement" ~: test
    [ fr [mk minBound 'a', mk 'y' maxBound] ~=? co (fr [mk 'b' 'x'])
    , fr [mk 'a' 'b', mk 'i' 'i', mk 'x' 'z'] ~=?
      coco (fr [mk 'a' 'b', mk 'i' 'i', mk 'x' 'z'])
    , empty ~=? co alphabet
    , alphabet ~=? co empty
    , empty ~=? coco empty
    , empty ~=? (co $ coco alphabet)
    ]
  where
    fr   = fromRanges
    mk   = mkRange
    co   = complement
    coco = co . co

t_union = "union" ~: test
    [ fr [mk 'a' 'a'] ~=? union (fr [mk 'a' 'a']) empty
    , fr [mk 'a' 'a'] ~=? union (fr [mk 'a' 'a']) (fr [mk 'a' 'a'])
    , fr [mk 'a' 'b'] ~=? union (fr [mk 'a' 'a']) (fr [mk 'b' 'b'])
    , fr [mk 'a' 'd'] ~=? union (fr [mk 'a' 'c']) (fr [mk 'b' 'd'])
    , fr [mk 'a' 'd'] ~=? union (fr [mk 'a' 'a']) (fr [mk 'b' 'd'])
    , fr [mk 'a' 'd'] ~=? union (fr [mk 'b' 'd']) (fr [mk 'a' 'a'])
    , fr [mk 'a' 'd'] ~=? union (fr [mk 'c' 'd']) (fr [mk 'a' 'b'])
    , fr [mk 'a' 'e'] ~=? union (fr [mk 'c' 'e']) (fr [mk 'a' 'd'])
    , fr [mk 'a' 'c', mk 'i' 'j'] ~=? union (fr [mk 'a' 'c'])
                                            (fr [mk 'i' 'j'])
    , fr [mk 'a' 'c', mk 'i' 'j'] ~=? union (fr [mk 'a' 'c'])
                                            (fr [mk 'i' 'j', mk 'b' 'c'])
    , fr [mk 'a' 'e', mk 'i' 'j', mk 'm' 'n'] ~=?
      union (fr [mk 'a' 'c', mk 'm' 'n']) (fr [mk 'i' 'j', mk 'b' 'e'])
    ]
  where
    fr = fromRanges
    mk = mkRange

t_intersect = "intersect" ~: test
    [ fr [mk 'a' 'a'] ~=? intersect (fr [mk 'a' 'a']) (fr [mk 'a' 'a'])
    , empty           ~=? intersect (fr [mk 'a' 'a']) (fr [mk 'b' 'b'])
    , fr [mk 'b' 'b'] ~=? intersect (fr [mk 'a' 'b']) (fr [mk 'b' 'c'])
    , fr [mk 'b' 'b'] ~=? intersect (fr [mk 'b' 'c']) (fr [mk 'a' 'b'])
    , fr [mk 'c' 'e'] ~=? intersect (fr [mk 'a' 'e']) (fr [mk 'c' 'x'])
    , fr [mk 'c' 'c'] ~=? intersect (fr [mk 'a' 'a', mk 'c' 'f'])
                                    (fr [mk 'b' 'c', mk 'g' 'i'])
    , fr [mk 'e' 'f'] ~=? intersect (fr [mk 'a' 'a', mk 'c' 'f'])
                                    (fr [mk 'b' 'b', mk 'e' 'i'])
    ]
  where
    fr = fromRanges
    mk = mkRange

suite = test
    [ t_mkRange, t_loR, t_hiR, t_alphabet_empty, t_fromRanges, t_toRanges
    , t_toPartition, t_fromPartition, t_member, t_complement, t_firstSymb
    , t_union, t_intersect
    ]

