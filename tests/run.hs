
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

suite = test [t_mkRange, t_fromRanges]

