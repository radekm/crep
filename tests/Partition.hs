import Test.HUnit

import Core.Partition
import Data.Word (Word8)

main :: IO ()
main = do runTestTT suite
          return ()

t_fromList_toList = "fromList, toList" ~: test
  [
    let xs = [(1, 'a'), (2, 'c'), (1, 'z'), (0, maxBound)]
    in toList ((fromList xs) :: PartitionL Char) ~=? xs
  , let xs = [(5, 32), (19, 65), (0, 69), (16, 92), (1, 255)]
    in toList ((fromList xs) :: PartitionL Word8) ~=? xs
  , let xs = [(0, 255)]
    in toList ((fromList xs) :: PartitionL Word8) ~=? xs
  ]

t_getBlock = "getBlock" ~: test
  [
    let pa :: PartitionL Char
        pa = fromList [(1, 'B'), (6, 'c'), (1, 'z'), (6, maxBound)]
    in (gb 'A' pa, gb 'B' pa, gb 'C' pa) ~=? (1, 1, 6)
  , let pa :: PartitionL Word8
        pa = fromList [(1, 5), (2, 6), (3, 7), (4, maxBound)]
    in (gb 4 pa, gb 5 pa, gb 6 pa, gb 7 pa, gb 8 pa) ~=? (1, 1, 2, 3, 4)
  , let pa :: PartitionL Word8
        pa = fromList [(1, 0), (2, 1), (3, 254), (4, maxBound)]
    in (gb 0 pa, gb 1 pa, gb 2 pa, gb 253 pa, gb 254 pa, gb 255 pa) ~=?
       (1, 2, 3, 3, 3, 4)
  ]
  where
    gb :: Pa p s => s -> p s -> BlockId
    gb = getBlock

--t_mergeWith

t_representatives = "representatives" ~: test
  [
    let pa :: PartitionL Char
        pa = fromList [(7, 'B'), (6, 'c'), (1, 'z'), (5, maxBound)]
    in representatives pa ~=? [(1, 'z'), (5, maxBound), (6, 'c'), (7, 'B')]
  , let pa :: PartitionL Word8
        pa = fromList [(1, 5), (3, 6), (1, 8), (3, 9), (1, maxBound)]
    in representatives pa ~=? [(1, 5), (3, 6)]
  , let pa :: PartitionL Word8
        pa = fromList [(15, maxBound)]
    in representatives pa ~=? [(15, maxBound)]
  ]

t_pmap = "pmap" ~: test
  [
    let pa :: PartitionL Word8
        pa = fromList [(6, 5), (7, 6), (1, 8), (5, 9), (4, maxBound)]
    in (mapList f1 pa, mapList f2 pa, mapList f3 pa) ~=?
       ([(7, 5), (8, 6), (2, 8), (6, 9), (5, maxBound)]
       ,[(9, maxBound)]
       ,[(9, 6), (1, 8), (9, maxBound)])
  , let pa :: PartitionL Word8
        pa = fromList [(4, 5), (3, 6), (2, 8), (3, 9), (0, maxBound)]
    in (mapList f1 pa, mapList f2 pa, mapList f3 pa) ~=?
       ([(5, 5), (4, 6), (3, 8), (4, 9), (1, maxBound)]
       ,[(9, maxBound)]
       ,[(9, 6), (4, 8), (9, 9), (0, maxBound)])
  ]
  where
    mapList f = toList . pmap f
    f1 = succ
    f2 = const 9
    f3 x | x `elem` [0..3] = x^2
         | otherwise       = 9

t_toIntervals = "toIntervals" ~: test
  [
    let pa :: PartitionL Word8
        pa = fromList [(6, 5), (7, 6), (1, 8), (5, 9), (4, maxBound)]
    in toIntervals pa ~=?
       [(6, 0, 5), (7, 6, 6), (1, 7, 8), (5, 9, 9), (4, 10, maxBound)]
  , let pa :: PartitionL Word8
        pa = fromList [(6, 0), (5, pred $ maxBound), (7, maxBound)]
    in toIntervals pa ~=?
       [(6, 0, 0), (5, 1, pred $ maxBound), (7, maxBound, maxBound)]
  ]

-- t_fromRanges
-- t_toRanges

t_alphabet_empty = "alphabet, empty" ~: test
  [ all (not . member' (empty    :: PartitionL Word8)) bytes ~=? True
  , all (      member' (alphabet :: PartitionL Word8)) bytes ~=? True
  ]
  where
    bytes   = [minBound..maxBound]
    member' = flip member

-- t_member

-- t_complement

-- t_union

-- t_intersect

suite = test
  [
    t_fromList_toList, t_getBlock, t_representatives, t_pmap, t_toIntervals
  , t_alphabet_empty
  ]

{-

OLD TESTS OF SYMBOL SET:

t_fromRangesC = "fromRanges (Char)" ~: test
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

t_toRangesC = "toRanges (Char)" ~: test
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

t_fromPartitionC = "fromPartition (Char)" ~:
    [ let x = fromRanges [mkRange minBound 'a']
      in mt [x, complement x] ~=?
         mt (fr (PC (toValueC 'a')
                    (PC (oneC .|. toValueC maxBound) NilC)))
    ]
  where
    fr = fromPartition
    to = toPartition
    mt = map to

t_memberC = "member (Char)" ~: test
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

t_firstSymbC = "firstSymb (Char)" ~: test
    [ 'a' ~=? firstSymb (fr [mk 'a' 'd'])
    , 'A' ~=? firstSymb (fr [mk 'a' 'd', mk 'A' 'b'])
    , '\0' ~=? firstSymb alphabet
    ]
  where
    mk = mkRange
    fr = fromRanges

t_complementC = "complement (Char)" ~: test
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

t_unionC = "union (Char)" ~: test
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

t_intersectC = "intersect (Char)" ~: test
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

t_fromRangesB = "fromRanges (byte)" ~: test
    [ fr [mk 12 12] ~=? fr [mk 12 12, mk 12 12]
    , fr [mk 12 13] ~=? fr [mk 12 12, mk 13 13]
    , fr [mk 12 13] ~=? fr [mk 13 13, mk 12 12]
    , fr [mk 12 13] ~=? fr [mk 13 13, mk 12 13]
    , fr [mk 12 13] ~=? fr [mk 12 13, mk 12 12]
    , fr [mk 10 90] ~=? fr [mk 41 57, mk 89 90, mk 60 65, mk 10 29, mk 30 31
                           ,mk 80 88, mk 66 79, mk 32 40, mk 58 59]
    , fr [mk 12 16, mk 95 95] ~=? fr [mk 14 15, mk 95 95, mk 12 13, mk 16 16]
    , fr [mk 12 16, mk 95 95] ~=? fr [mk 12 15, mk 95 95, mk 12 16]
    , alphabet ~=? fr [mk 15 27, mk 0 14, mk 27 255]
    , alphabet ~=? fr [mk 15 27, mk 0 15, mk 28 255]
    , alphabet ~=? fr [mk 15 27, mk 0 14, mk 28 30, mk 31 43, mk 42 255]
    , fr [mk 0 10, mk 35 45, mk 243 255] ~=?
      fr [mk 243 244, mk 10 10, mk 36 42, mk 245 255, mk 0 9, mk 42 45
         ,mk 35 35]
    ]
  where
    mk = (mkRange :: Word8 -> Word8 -> Range Word8)
    fr = fromRanges

t_toRangesB = "toRanges (byte)" ~: test
    [ [mk 0 5, mk 12 32, mk 83 108] ~=?
      to (fr [mk 12 25, mk 26 32, mk 0 5, mk 5 5, mk 83 106, mk 106 108])
    , [mk 10 12, mk 254 255] ~=?
      to (fr [mk 11 12, mk 255 255, mk 10 10, mk 254 254])
    ]
  where
    mk = (mkRange :: Word8 -> Word8 -> Range Word8)
    fr = fromRanges
    to = toRanges

t_toPartitionB = "toPartition (byte)" ~: test
    [ (PB 7 $ PB (10 .|. oneB) $ PB 19 $ PB (25 .|. oneB) $ PB 255 NilB) ~=?
      toPartition (fromRanges [mk 20 25, mk 9 10, mk 8 8])
    , 266 ~=? (10 .|. oneB)
    , 281 ~=? (25 .|. oneB)
    ]
  where
    mk = (mkRange :: Word8 -> Word8 -> Range Word8)

t_fromPartitionB = "fromPartition (byte)" ~: test
    [ fromPartition
      ]
  where
    mk = (mkRange :: Word8 -> Word8 -> Range Word8)

-}
