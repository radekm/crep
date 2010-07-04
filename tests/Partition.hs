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

t_fromRanges = "fromRanges" ~:
  [
    fr [r 'a' 'a'] ~=? fr [r 'a' 'a', r 'a' 'a']
  , fr [r 'a' 'b'] ~=? fr [r 'a' 'a', r 'b' 'b']
  , fr [r 'a' 'd'] ~=? fr [r 'a' 'b', r 'c' 'd']
  , fr [r 'a' 'e'] ~=? fr [r 'a' 'c', r 'b' 'e']
  , fr [r 'a' 'e'] ~=? fr [r 'a' 'b', r 'b' 'b', r 'd' 'e', r 'c' 'c']
  , fr [r 'c' 'd', r 'a' 'a'] ~=? fr [r 'a' 'a', r 'd' 'd', r 'c' 'c']
  , fr [r 'a' 'g', r 'A' 'a', r 'c' 'd', r 'i' 'l', r 'm' 'z'] ~=?
    fr [r 'A' 'g', r 'i' 'z']

  , fr' [r 12 12] ~=? fr' [r 12 12, r 12 12]
  , fr' [r 12 13] ~=? fr' [r 12 12, r 13 13]
  , fr' [r 12 13] ~=? fr' [r 13 13, r 12 12]
  , fr' [r 12 13] ~=? fr' [r 13 13, r 12 13]
  , fr' [r 12 13] ~=? fr' [r 12 13, r 12 12]
  , fr' [r 10 90] ~=? fr' [r 41 57, r 89 90, r 60 65, r 10 29, r 30 31
                          ,r 80 88, r 66 79, r 32 40, r 58 59]
  , fr' [r 12 16, r 95 95] ~=? fr' [r 14 15, r 95 95, r 12 13, r 16 16]
  , fr' [r 12 16, r 95 95] ~=? fr' [r 12 15, r 95 95, r 12 16]
  , alphabet' ~=? fr' [r 15 27, r 0 14, r 27 255]
  , alphabet' ~=? fr' [r 15 27, r 0 15, r 28 maxBound]
  , alphabet' ~=? fr' [r 15 27, r 0 14, r 28 30, r 31 43, r 42 255]
  , fr' [r 0 10, r 35 45, r 243 255] ~=?
    fr' [r 243 244, r 10 10, r 36 42, r 245 255, r 0 9, r 42 45, r 35 35]
  ]
  where
    alphabet' = toList (alphabet :: PartitionL Word8)
    fr  = toList . (fromRanges :: [Range Char]  -> PartitionL Char)
    fr' = toList . (fromRanges :: [Range Word8] -> PartitionL Word8)
    r   = Range

t_toRanges = "toRanges" ~: test
  [
    [r 'a' 'a'] ~=? to (fr [r 'a' 'a'])
  , [r 'a' 'b'] ~=? to (fr [r 'a' 'b'])
  , [r 'a' 'b'] ~=? to (fr [r 'a' 'a', r 'b' 'b'])
  , [r 'a' 'e'] ~=?
    to (fr [r 'a' 'a', r 'd' 'd', r 'e' 'e', r 'c' 'c', r 'b' 'b'])
  , [r 'F' 'M', r 'c' 'd'] ~=? to (fr [r 'c' 'd', r 'F' 'M'])
  , let x = fr [r 'g' 'j', r '0' '8', r 'a' 'b', r 'c' 'd', r 'X' 'X']
    in toList x ~=? toList (fr $ to x)

  , [r 0 5, r 12 32, r 83 108] ~=?
    to' (fr' [r 12 25, r 26 32, r 0 5, r 5 5, r 83 106, r 106 108])
  , [r 10 12, r 254 255] ~=?
    to' (fr' [r 11 12, r 255 255, r 10 10, r 254 254])
  ]
  where
    to  = toRanges :: PartitionL Char -> [Range Char]
    fr  = fromRanges :: [Range Char] -> PartitionL Char
    to' = toRanges :: PartitionL Word8 -> [Range Word8]
    fr' = fromRanges :: [Range Word8] -> PartitionL Word8
    r   = Range

t_alphabet_empty = "alphabet, empty" ~: test
  [
    all (not . member' (empty    :: PartitionL Word8)) bytes ~=? True
  , all (      member' (alphabet :: PartitionL Word8)) bytes ~=? True
  ]
  where
    bytes   = [minBound..maxBound]
    member' = flip member

t_member = "member" ~: test
  [
    True ~=? member 'a' (fr [r 'a' 'a'])
  , True ~=? member 'c' (fr [r 'a' 'z'])
  , True ~=? member 'c' (fr [r 'A' 'B', r 'C' 'E', r 'a' 'c'])
  , True ~=? member 'c' (fr [r 'A' 'F', r 'c' 'e'])
  , False ~=? member 'c' (fr [r 'A' 'F', r 'a' 'b'])
  , False ~=? member 'c' (fr [r 'A' 'F', r 'a' 'b', r 'd' 'z'])
  ]
  where
    fr = fromRanges :: [Range Char] -> PartitionL Char
    r  = Range

t_complement = "complement" ~: test
  [
    (toList $ fr [r minBound 'a', r 'y' maxBound]) ~=?
    (toList $ co (fr [r 'b' 'x']))
  , (toList $ fr [r 'a' 'b', r 'i' 'i', r 'x' 'z']) ~=?
    (toList $ co $ co (fr [r 'a' 'b', r 'i' 'i', r 'x' 'z']))
  , toList alphabet' ~=? (toList $ co empty')
  , toList empty' ~=? (toList $ co alphabet')
  , toList empty' ~=? (toList $ co $ co empty')
  , toList empty' ~=? (toList $ co $ co $ co alphabet')
  , toList empty' ~=?
    (toList $ co $ pmap succ $ fr [r minBound 'a', r 'y' maxBound])
  ]
  where
    fr   = fromRanges :: [Range Char] -> PartitionL Char
    r    = Range
    co   = complement
    alphabet' = alphabet :: PartitionL Char
    empty'    = empty :: PartitionL Char

t_union = "union" ~: test
  [
    frL [r 'a' 'a'] ~=? unionL (fr [r 'a' 'a']) empty
  , frL [r 'a' 'a'] ~=? unionL (fr [r 'a' 'a']) (fr [r 'a' 'a'])
  , frL [r 'a' 'b'] ~=? unionL (fr [r 'a' 'a']) (fr [r 'b' 'b'])
  , frL [r 'a' 'd'] ~=? unionL (fr [r 'a' 'c']) (fr [r 'b' 'd'])
  , frL [r 'a' 'd'] ~=? unionL (fr [r 'a' 'a']) (fr [r 'b' 'd'])
  , frL [r 'a' 'd'] ~=? unionL (fr [r 'b' 'd']) (fr [r 'a' 'a'])
  , frL [r 'a' 'd'] ~=? unionL (fr [r 'c' 'd']) (fr [r 'a' 'b'])
  , frL [r 'a' 'e'] ~=? unionL (fr [r 'c' 'e']) (fr [r 'a' 'd'])
  , frL [r 'a' 'c', r 'i' 'j'] ~=? unionL (fr [r 'a' 'c'])
                                          (fr [r 'i' 'j'])
  , frL [r 'a' 'c', r 'i' 'j'] ~=? unionL (fr [r 'a' 'c'])
                                          (fr [r 'i' 'j', r 'b' 'c'])
  , frL [r 'a' 'e', r 'i' 'j', r 'm' 'n'] ~=?
    unionL (fr [r 'a' 'c', r 'm' 'n']) (fr [r 'i' 'j', r 'b' 'e'])
  ]
  where
    fr         = fromRanges :: [Range Char] -> PartitionL Char
    frL        = toList . fr
    unionL a b = toList $ union a b
    r          = Range

t_intersect = "intersect" ~: test
  [
    frL [r 'a' 'a'] ~=? intersectL (fr [r 'a' 'a']) (fr [r 'a' 'a'])
  , toList empty'   ~=? intersectL (fr [r 'a' 'a']) (fr [r 'b' 'b'])
  , frL [r 'b' 'b'] ~=? intersectL (fr [r 'a' 'b']) (fr [r 'b' 'c'])
  , frL [r 'b' 'b'] ~=? intersectL (fr [r 'b' 'c']) (fr [r 'a' 'b'])
  , frL [r 'c' 'e'] ~=? intersectL (fr [r 'a' 'e']) (fr [r 'c' 'x'])
  , frL [r 'c' 'c'] ~=? intersectL (fr [r 'a' 'a', r 'c' 'f'])
                                   (fr [r 'b' 'c', r 'g' 'i'])
  , frL [r 'e' 'f'] ~=? intersectL (fr [r 'a' 'a', r 'c' 'f'])
                                   (fr [r 'b' 'b', r 'e' 'i'])
  ]
  where
    fr             = fromRanges :: [Range Char] -> PartitionL Char
    frL            = toList . fr
    intersectL a b = toList $ intersect a b
    empty'         = empty :: PartitionL Char
    r              = Range

suite = test
  [
    t_fromList_toList, t_getBlock, t_representatives, t_pmap, t_toIntervals
  , t_fromRanges, t_toRanges, t_alphabet_empty, t_member, t_complement
  , t_union, t_intersect
  ]
