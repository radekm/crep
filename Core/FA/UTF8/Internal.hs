-- |
-- Module    : Core.FA.UTF8.Internal
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
-- Converts automaton working on Unicode code points to automaton working
-- on 8-bit bytes which recognizes same words encoded in UTF-8.
--
module Core.FA.UTF8.Internal where

import Control.Arrow (first, second)
import Data.Word (Word32)
import Data.Bits hiding (complement)
import Core.SymbSet
import Core.FA.DFA

-- |At least 32-bit number which can hold Unicode code point or UTF-8
-- representation of Unicode code point.
type Code = Word32

type Branch = (ByteSet, Machine)

type Unused = ByteSet

-- |N-ary tree representing finite state machine.
data Machine = Fork [Branch] Unused
             | Leaf StNum
             deriving Show

emptyMachine :: Machine
emptyMachine = Fork [] alphabet

-- |Splits each range from the given list by the number of bytes in their
-- UTF-8 representation. Returns 4 lists where i-th list contains characters
-- which have i bytes in UTF-8 representation.
splitRanges :: [(Range Char, StNum)]
            -> ([(Range Char, StNum)], [(Range Char, StNum)]
               ,[(Range Char, StNum)], [(Range Char, StNum)])
splitRanges ((r, s):rs)
  | l <= c1
  = case h of
      _ | h <= c1   -> ((r, s):x1, x2, x3, x4)
        | h <= c2   -> ((mk l c1, s):x1, (mk c1' h, s):x2, x3, x4)
        | h <= c3   -> ((mk l  c1, s):x1, (mk c1' c2, s):x2
                       ,(mk c2' h, s):x3,                x4)
        | otherwise -> ((mk l   c1, s):x1, (mk c1' c2, s):x2
                       ,(mk c2' c3, s):x3, (mk c3' h,  s):x4)
  | l <= c2
  = case h of
      _ | h <= c2   -> (x1, (r, s):x2, x3, x4)
        | h <= c3   -> (x1, (mk l c2, s):x2, (mk c2' h, s):x3, x4)
        | otherwise -> (               x1, (mk l  c2, s):x2
                       ,(mk c2' c3, s):x3, (mk c3' h, s):x4)
  | l <= c3
  = case h of
      _ | h <= c3   -> (x1, x2, (r,       s):x3,               x4)
        | otherwise -> (x1, x2, (mk l c3, s):x3, (mk c3' h, s):x4)
  | otherwise
                     = (x1, x2, x3, (r, s):x4)
  where
    l  = loR r
    h  = hiR r
    mk = mkRange
    (c1,  c2,  c3)  = ('\127', '\2047', '\65535')
    (c1', c2', c3') = ('\128', '\2048', '\65536')
    (x1, x2, x3, x4) = splitRanges rs
splitRanges [] = ([], [], [], [])

-- |Combines 4 bytes and creates code. 
bytesToCode :: Code -> Code -> Code -> Code -> Code
bytesToCode a b c d = shiftL (shiftL (shiftL a 8 .|. b) 8 .|. c) 8 .|. d

utf8code1 :: Code -> Code
utf8code1 = id

utf8code2 :: Code -> Code
utf8code2 c = bytesToCode 0 0 (192 + shiftR c 6) (128 + (c .&. 63))

utf8code3 :: Code -> Code
utf8code3 c = bytesToCode 0 (224 + shiftR c 12)
                            (128 + (shiftR c 6 .&. 63))
                            (128 + (c .&. 63))

utf8code4 :: Code -> Code
utf8code4 c = bytesToCode (240 + shiftR c 18)
                          (128 + (shiftR c 12 .&. 63))
                          (128 + (shiftR c  6 .&. 63))
                          (128 + (c .&. 63))

toMachine1 :: [(Range Char, StNum)] -> Machine
toMachine1 = rangesToMachine utf8code1 1

toMachine2 :: [(Range Char, StNum)] -> Machine
toMachine2 = rangesToMachine utf8code2 2

toMachine3 :: [(Range Char, StNum)] -> Machine
toMachine3 = rangesToMachine utf8code3 3

toMachine4 :: [(Range Char, StNum)] -> Machine
toMachine4 = rangesToMachine utf8code4 4

rangesToMachine :: (Code -> Code) -> Int -> [(Range Char, StNum)] -> Machine
rangesToMachine utf8code nBytes = foldl f emptyMachine
  where
    f m (rng, st)
      = addMachine m (toMachine (utf8code $ fromIntegral $ fromEnum $ loR rng)
                                (utf8code $ fromIntegral $ fromEnum $ hiR rng)
                                nBytes st BetweenMinMax)

data Bound = BetweenMinMax
           | MinOrMore
           | MaxOrLess
           | NoBound
           deriving Show

-- |Converts Unicode range to machine which recognizes characters from that
-- range.
toMachine :: Code -> Code -> Int -> StNum -> Bound -> Machine
toMachine minBytes maxBytes nByte st bound
  | nByte > 0
  = case bound of
      MinOrMore
        | minByte == u8max
        -> Fork [(rngMin, cont MinOrMore)]
                unusedMinOrMore
        | otherwise
        -> Fork [(rngMin, cont MinOrMore)
                ,(mk (minByte+1) u8max, cont NoBound)]
                unusedMinOrMore
      MaxOrLess
        | maxByte == u8min
        -> Fork [(rngMax, cont MaxOrLess)]
                unusedMaxOrLess
        | otherwise
        -> Fork [(rngMax, cont MinOrMore)
                ,(mk u8min (maxByte-1), cont NoBound)]
                unusedMaxOrLess
      NoBound
        -> Fork [(mk u8min u8max, cont NoBound)]
                unusedNoBound
      BetweenMinMax
        | minByte == maxByte
        -> Fork [(rngMin, cont BetweenMinMax)]
                unusedBetweenMinMax
        | minByte+1 == maxByte
        -> Fork [(rngMin, cont MinOrMore)
                ,(rngMax, cont MaxOrLess)]
                unusedBetweenMinMax
        | otherwise
        -> Fork [(rngMin, cont MinOrMore)
                ,(rngMax, cont MaxOrLess)
                ,(mk (minByte+1) (maxByte-1), cont NoBound)]
                unusedBetweenMinMax
  -- Last byte of the code point.
  | otherwise
  = case bound of
      MinOrMore     -> Fork [(mk minByte u8max,   Leaf st)]
                            unusedMinOrMore
      MaxOrLess     -> Fork [(mk u8min maxByte,   Leaf st)]
                            unusedMaxOrLess
      NoBound       -> Fork [(mk u8min u8max,     Leaf st)]
                            unusedNoBound
      BetweenMinMax -> Fork [(mk minByte maxByte, Leaf st)]
                            unusedBetweenMinMax
  where
    -- Extract bytes.
    minByte = getByte minBytes
    maxByte = getByte maxBytes
    getByte = fromIntegral . (`shiftL` (nByte * 8))

    -- Minimum and maximum for 2nd, 3rd and 4th byte.
    u8min = 128 -- binary: 10000000
    u8max = 191 -- binary: 10111111

    mk l h = fromRanges [mkRange l h]

    -- Interval with minimal (maximal) byte.
    rngMin = mk minByte minByte
    rngMax = mk maxByte maxByte

    -- Continue.
    cont = toMachine minBytes maxBytes (nByte-1) st

    -- Which bytes are not used.
    unusedMinOrMore     = fromRanges [mkRange minBound (minByte-1)
                                     ,mkRange (u8max+1) maxBound]
    unusedMaxOrLess     = fromRanges [mkRange minBound (u8min-1)
                                     ,mkRange (maxByte+1) maxBound]
    unusedNoBound       = fromRanges [mkRange minBound (u8min-1)
                                     ,mkRange (u8max+1) maxBound]
    unusedBetweenMinMax = fromRanges [mkRange minBound (minByte-1)
                                     ,mkRange (maxByte+1) maxBound]

-- |The function @('addMachine' x m)@ extends machine @m@.
addMachine :: Machine -> Machine -> Machine
addMachine (Fork (b:bs) unused) = addMachine (Fork bs unused) . addBranch b
addMachine (Fork [] _)          = id
addMachine _                    = error "addMachine: overlap"

-- |The function @('addBranch b m)@ extends machine @m@.
addBranch :: Branch -> Machine -> Machine
addBranch (bytes, machine) (Fork bs unused)
  | unused' /= empty = Fork (newBranch:branches ++ cobranches) counused'
  | otherwise        = Fork (          branches ++ cobranches) unused
  where
    cobytes = complement bytes
    
    intersectBsWith byteSet = filter ((/=) empty . fst) 
                                $ map (first (intersect byteSet)) bs

    branches   = map (second (addMachine machine)) $ intersectBsWith bytes
    cobranches = intersectBsWith cobytes

    unused'   = unused `intersect` bytes
    counused' = unused `intersect` cobytes

    newBranch = (unused', machine)
addBranch _ _ = error "addBranch: overlap"

