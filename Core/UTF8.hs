-- |
-- Module    : Core.UTF8
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
module Core.UTF8 where

import Data.Word (Word8, Word)
import Core.Partition
import Data.Bits ((.&.), shiftL, shiftR)

type Sequence = [Range Word8]

convertRange :: Range Char -> [Sequence]
convertRange rng
  = concatMap (\r -> bytesToRanges (fst r) (snd r) limits Both) bytes
  where
    (r1, r2, r3, r4) = splitRange rng

    bytes :: [([Word8], [Word8])]
    bytes = map (toBytes charToBytes1) r1 ++ map (toBytes charToBytes2) r2 ++
            map (toBytes charToBytes3) r3 ++ map (toBytes charToBytes4) r4

    toBytes :: (Char -> [Word8]) -> Range Char -> ([Word8], [Word8])
    toBytes f (Range a b) = (f a, f b)

-- | Splits given range of characters into four subranges. UTF-8 codes
--   of characters in n-th subrange consist of n bytes.
splitRange :: Range Char
            -> ([Range Char], [Range Char]
               ,[Range Char], [Range Char])
splitRange r@(Range l h)
  | l <= c1
  = case h of
      _ | h <= c1   -> ([r], [], [], [])
        | h <= c2   -> ([Range l c1], [Range c1' h], [], [])
        | h <= c3   -> ([Range l c1], [Range c1' c2]
                       ,[Range c2' h], [])
        | otherwise -> ([Range l c1], [Range c1' c2]
                       ,[Range c2' c3], [Range c3' h])
  | l <= c2
  = case h of
      _ | h <= c2   -> ([], [r], [], [])
        | h <= c3   -> ([], [Range l c2], [Range c2' h], [])
        | otherwise -> ([], [Range l c2], [Range c2' c3], [Range c3' h])
  | l <= c3
  = case h of
      _ | h <= c3   -> ([], [], [r],[])
        | otherwise -> ([], [], [Range l c3], [Range c3' h])
  | otherwise
                     = ([], [], [], [r])
  where
    (c1,  c2,  c3)  = ('\127', '\2047', '\65535')
    (c1', c2', c3') = ('\128', '\2048', '\65536')

bits :: Word -> Int -> Int -> Word8
bits w shift nBits = fromIntegral ((w `shiftR` shift) .&. mask)
  where
    mask = (1 `shiftL` nBits) - 1

charToBytes1 :: Char -> [Word8]
charToBytes1 c = [bits w 0 7]
  where
    w = fromIntegral $ fromEnum c

charToBytes2 :: Char -> [Word8]
charToBytes2 c = [192 + bits w 6 5, 128 + bits w 0 6]
  where
    w = fromIntegral $ fromEnum c

charToBytes3 :: Char -> [Word8]
charToBytes3 c = [224 + bits w 12 4, 128 + bits w 6 6, 128 + bits w 0 6]
  where
    w = fromIntegral $ fromEnum c

charToBytes4 :: Char -> [Word8]
charToBytes4 c = [ 240 + bits w 18 3, 128 + bits w 12 6
                 , 128 + bits w 6  6, 128 + bits w 0  6 ]
  where
    w = fromIntegral $ fromEnum c

limits :: [(Word8, Word8)]
limits = (error "limits-lo", error "limits-hi") : repeat (128, 191)

data Bounds = Min | Max | Both | None

bytesToRanges :: [Word8]
              -> [Word8]
              -> [(Word8, Word8)]
              -> Bounds
              -> [Sequence]
bytesToRanges [] _ _ _ = []
bytesToRanges (l:ls) (h:hs) ((min', max'):lims) bounds
  = case bounds of
      None -> next (Range min' max') None
      Min
        | max' == l -> lNext
        | otherwise -> lNext ++ next (Range l' max') None
      Max
        | min' == h -> hNext
        | otherwise -> hNext ++ next (Range min' h') None
      Both
        | l == h    -> lNext {- == hNext -}
        | l' > h'   -> lNext ++ hNext
        | otherwise -> lNext ++ hNext ++ next (Range l' h') None
  where
    l' = succ l
    h' = pred h
    lNext = next (Range l l) Min
    hNext = next (Range h h) Max
    next rng newBounds = map (rng:) $ bytesToRanges ls hs lims newBounds
bytesToRanges _ _ _ _ = error "Core.UTF8.bytesToRanges: bad input"
