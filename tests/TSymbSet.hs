
import Test.HUnit

import Core.SymbSet

main :: IO ()
main = do runTestTT suite
          return ()

t_mkRange = [ mk 'a' 'a' ~=? mk 'a' 'a'
            , mk 'a' 'b' ~=? mk 'a' 'b'
            , False ~=? (mk 'a' 'a' == mk 'A' 'A')
            , False ~=? (mk 'a' 'b' == mk 'a' 'c')
            ]
  where
    mk = mkRange

t_fromRanges
  = [ fr [mk 'a' 'a'] ~=? fr [mk 'a' 'a']
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

t_member = [ True ~=? mem 'a' (fr [mk 'a' 'a'])
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

t_complement
  = [ fr [mk minBound 'a', mk 'y' maxBound] ~=? co (fr [mk 'b' 'x'])
    , fr [mk 'a' 'b', mk 'i' 'i', mk 'x' 'z'] ~=?
      co (co (fr [mk 'a' 'b', mk 'i' 'i', mk 'x' 'z']))
    ]
  where
    fr = fromRanges
    mk = mkRange
    co = complement

t_union
  = [ fr [mk 'a' 'a'] ~=? union (fr [mk 'a' 'a']) empty
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
    fr  = fromRanges
    mk  = mkRange

t_intersect
  = [ fr [mk 'a' 'a'] ~=? intersect (fr [mk 'a' 'a']) (fr [mk 'a' 'a'])
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
    fr  = fromRanges
    mk  = mkRange

suite = test [ t_mkRange, t_fromRanges, t_member, t_complement, t_union
             , t_intersect
             ]

