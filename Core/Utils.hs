-- |
-- Module    : Core.Utils
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Set of symbols and range of symbols.
--
module Core.Utils where

import Numeric (showHex)
import Data.Maybe (fromJust)

-- |List of the characters which have single-character escape code.
escapeChars :: String
escapeChars = "\0\a\b\f\n\r\t\v\ESC"

-- |List of escape codes. The escape codes correspond to the characters
-- in @'escapeChars'@.
escapeCodes :: String
escapeCodes = "0abfnrtve"

-- |Escapes special character. Special character is backslash and
-- every character with code lower than 32.
escapeSpecial :: Char -> String
escapeSpecial c
  | c == '\\'            = "\\\\"
  | c `elem` escapeChars = '\\':lookup' (zip escapeChars escapeCodes) c:""
  | code < 32            = "\\x" ++ replicate (2 - length hex) '0' ++ hex
  | otherwise            = [c]
  where
    code = fromEnum c
    hex  = showHex code ""

-- |Like @'lookup'@ but unsafe with flipped arguments.
lookup' :: (Eq a) => [(a, b)] -> a -> b
lookup' alist = fromJust . flip lookup alist

