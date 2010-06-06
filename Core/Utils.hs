-- |
-- Module    : Core.Utils
-- Copyright : (c) Radek Micek 2009, 2010
-- License   : BSD3
-- Stability : experimental
--
-- Utility functions.
--
module Core.Utils where

import Numeric (showHex)
import Data.Maybe (fromJust)
import Data.List (group)

-- | Removes duplicate items from the given sorted list.
nubSorted :: Eq a => [a] -> [a]
nubSorted = map head . group

-- | Sorts list and removes duplicates. Function @f@ is applied to items
--   before comparsion.
--
-- @
-- 'sortAndNubWith' f == 'Data.List.nubBy'  (\\a b -> fa == fb) .
--                     'Data.List.sortBy' (\\a b -> compare (f a) (f b)
-- @
sortAndNubWith :: Ord b => (a -> b) -> [a] -> [a]
sortAndNubWith f = mergeSort . map (:[])
  where
    mergeSort []  = []
    mergeSort [a] = a
    mergeSort xs  = mergeSort $ mergePairs xs

    mergePairs []       = []
    mergePairs [a]      = [a]
    mergePairs (a:b:xs) = merge a b:mergePairs xs

    merge ass@(a:as) bss@(b:bs)
      = case compare (f a) (f b) of
          LT -> a:merge as  bss
          GT -> b:merge ass bs
          EQ -> a:merge as  bs  -- Duplicate is removed here.
    merge as@(_:_) [] = as
    merge []       bs = bs

-- | List of white characters.
whiteChars :: String
whiteChars = " \t\n\r\f\v"

-- | List of the characters which have single-character escape code.
escapeChars :: String
escapeChars = "\0\a\b\f\n\r\t\v\ESC"

-- | List of escape codes. The escape codes correspond to the characters
--   in 'escapeChars'.
escapeCodes :: String
escapeCodes = "0abfnrtve"

-- | Escapes special character. Special character is a backslash and
--   every character with code lower than 32.
escapeSpecial :: Char -> String
escapeSpecial c
  | c == '\\'            = "\\\\"
  | c `elem` escapeChars = '\\':lookup' (zip escapeChars escapeCodes) c:""
  | code < 32            = "\\x" ++ replicate (2 - length hex) '0' ++ hex
  | otherwise            = [c]
  where
    code = fromEnum c
    hex  = showHex code ""

-- | Like 'lookup' but unsafe with flipped arguments.
lookup' :: (Eq a) => [(a, b)] -> a -> b
lookup' alist = fromJust . flip lookup alist

-- | @'safeFoldl1' e f xs@ returns @'foldl1' f xs@ when @xs@ is nonempty
--   otherwise returns @e@.
safeFoldl1 :: a -> (a -> a -> a) -> [a] -> a
safeFoldl1 e _ [] = e
safeFoldl1 _ f xs = foldl1 f xs
