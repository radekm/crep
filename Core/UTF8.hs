{-# LANGUAGE GADTs #-}

-- |
-- Module    : Core.UTF8
-- Copyright : (c) Radek Micek 2010
-- License   : BSD3
-- Stability : experimental
--
module Core.UTF8
       (
         Sequence
       , convertRule
       , convertRegex
       , convertRange
       , convertSubst
       , convertString
       , convertChar
       , splitRange
       , charToBytes1
       , charToBytes2
       , charToBytes3
       , charToBytes4
       , bytesToChar
       , SeqTree(..)
       , sequencesToSeqTree
       ) where

import Data.Word (Word8, Word)
import Core.Set
import Core.Partition (toIntervals)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Core.Regex
import Core.Rule
import Data.Monoid (mconcat)
import Core.Utils (sortAndGroupBySnd)

type Sequence = [Range Word8]

convertRule :: Rule Char -> Rule Word8
convertRule (Rule name prio prefLen regex subst)
  = Rule name prio prefLen (convertRegex regex) (convertSubst subst)

convertRegex :: Regex Char c -> Regex Word8 c
convertRegex Epsilon = Epsilon
convertRegex (CharClass cs) = convertSymbSet cs
  where
    convertSymbSet :: Set Char -> Regex Word8 c
    convertSymbSet = seqTreeToRegex . sequencesToSeqTree
                                    . concatMap convertRange
                                    . toRanges
convertRegex (Or a b) = Or (convertRegex a) (convertRegex b)
convertRegex (And a b) = And (convertRegex a) (convertRegex b)
convertRegex (Concat a b) = Concat (convertRegex a) (convertRegex b)
convertRegex (RepeatU lo a) = RepeatU lo (convertRegex a)
convertRegex (Repeat lo hi a) = Repeat lo hi (convertRegex a)
convertRegex (Not a) = And (Not $ convertRegex a)
                           (convertRegex $ RepeatU 0 $ CharClass alphabet)
convertRegex (Capture i a) = Capture i (convertRegex a)

convertSubst :: Subst Char -> Subst Word8
convertSubst (Subst s) = Subst $ conv s
  where
    conv []              = []
    conv (TConst cs:xs)  = TConst (convertString cs):conv xs
    conv (TCapture i:xs) = TCapture i:conv xs

convertString :: String -> [Word8]
convertString = concatMap convertChar

convertChar :: Char -> [Word8]
convertChar c
  | c <= c1   = charToBytes1 c
  | c <= c2   = charToBytes2 c
  | c <= c3   = charToBytes3 c
  | otherwise = charToBytes4 c
  where
    (c1,  c2,  c3) = ('\127', '\2047', '\65535')

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

bytesToChar :: [Word8] -> Char
bytesToChar xs
  | y < 128          = remainingBytes y ys
  | y .&. 224 == 192 = remainingBytes (y .&. mask 5) ys
  | y .&. 240 == 224 = remainingBytes (y .&. mask 4) ys
  | y .&. 248 == 240 = remainingBytes (y .&. mask 3) ys
  | otherwise        = error "Core.UTF8.bytesToChar: invalid UTF-8 sequence"
  where
    (y:ys) = map (toEnum . fromEnum) xs

    remainingBytes :: Word -> [Word] -> Char
    remainingBytes w [] = toEnum $ fromEnum w
    remainingBytes w (b:bs)
      = remainingBytes ((w `shiftL` 6) .|. (b .&. mask 6)) bs

    mask :: Int -> Word
    mask n = (1 `shiftL` n) - 1

limits :: [(Word8, Word8)]
limits = (error "limits-lo", error "limits-hi") : repeat (128, 191)

data Bounds = Min | Max | Both | None

bytesToRanges :: [Word8]
              -> [Word8]
              -> [(Word8, Word8)]
              -> Bounds
              -> [Sequence]
bytesToRanges [] _ _ _ = [[]]
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
        | l == h    -> bNext
        | l' > h'   -> lNext ++ hNext
        | otherwise -> lNext ++ hNext ++ next (Range l' h') None
  where
    l' = succ l
    h' = pred h
    bNext = next (Range l h) Both  -- When h == l.
    lNext = next (Range l l) Min
    hNext = next (Range h h) Max
    next rng newBounds = map (rng:) $ bytesToRanges ls hs lims newBounds
bytesToRanges _ _ _ _ = error "Core.UTF8.bytesToRanges: bad input"

-- | Represents regular expression composed of character classes,
--   disjunction and concatenation.
data SeqTree = Fork [(Set Word8, SeqTree)]
             | Leaf
             deriving (Eq, Ord)

instance Show SeqTree where
  show (Fork xs) = "Fork " ++ show (map (\(p, tr) -> (toRanges p, tr)) xs)
  show Leaf      = "Leaf"

-- | Each sequence is nonempty or each sequence is empty.
sequencesToSeqTree :: [Sequence] -> SeqTree
sequencesToSeqTree [] = Leaf
sequencesToSeqTree seqs
  | any null seqs = Leaf
sequencesToSeqTree seqs = Fork branches'
  where
    firstRanges :: [Range Word8]
    firstRanges = map head seqs

    firstSymbSets :: [Set Word8]
    firstSymbSets = map (\range -> fromRanges [range]) firstRanges

    allChars = foldl1 union firstSymbSets

    symbSetsByRngs :: [Set Word8]
    symbSetsByRngs = map (\(_, l, h) -> fromRanges [Range l h])
                       $ toIntervals
                       $ mconcat
                       $ map toPartition firstSymbSets

    -- Remove symbol sets which contain no characters.
    symbSets = filter (\set -> intersect set allChars /= empty) symbSetsByRngs

    selectSeqs set
      = filter (\(rng:_) -> intersect (fromRanges [rng]) set /= empty ) seqs

    rmFirstRange = map tail

    branches = map (\set -> (set, sequencesToSeqTree $ rmFirstRange $
                                  selectSeqs set)) symbSets

    -- Merge same branches.
    branches' = map (\br -> (foldl1 union $ map fst br, snd $ head br)) $
                sortAndGroupBySnd branches

seqTreeToRegex :: SeqTree -> Regex Word8 c
seqTreeToRegex Leaf = Epsilon
seqTreeToRegex (Fork bs)
  = foldl1 Or (map (\(set, tr) -> CharClass set `Concat`
                                  seqTreeToRegex tr)
                   bs)
