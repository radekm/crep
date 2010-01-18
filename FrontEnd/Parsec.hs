-- |
-- Module    : FrontEnd.Parsec
-- Copyright : (c) Radek Micek 2009
-- License   : BSD3
-- Stability : experimental
--
-- Functions for parsing.
--
module FrontEnd.Parsec
  ( Parsec
  , ParseError
  , char
  , char_
  , noneOf
  , noneOf_
  , number
  , number_
  , escapeSeq
  , escapeSeq_
  , digit
  , skipSpacesAndComments
  , sepBy
  , sepBy1
  , many
  , many1
  , eof
  , between
  , choice
  , option
  , optionMaybe
  , try
  , unexpected
  , getState
  , putState
  , runParser
  , (<$>)
  , (<|>)
  , (<*>)
  , (<*)
  , (*>)
  , (<**>)
  , (<?>)
  ) where

import Numeric (readDec, readHex, showHex)
import Data.Char (ord)

import Text.Parsec hiding ((<|>), many, space, spaces)
import Control.Applicative hiding (optional, empty)
import Control.Monad (when)

import Core.Utils

-- |Parses number.
genericNumber
  :: (Parsec String st Char)          -- Parses one digit.
  -> (String -> [(Integer, String)])  -- Converts digits to number.
  -> (Integer -> String)              -- Converts number to string.
  -> String                           -- Name what is being read.
  -> Int                              -- Minimal value.
  -> Int                              -- Maximal value.
  -> Parsec String st Int
genericNumber digit' read' show' name minVal maxVal
  = try (do i <- liftA (fst . head . read') (many1 digit')
            when (i < minV || i > maxV) (unexpected $ show' i)
            return $ fromInteger i)
    <?> desc
  where
    minV = fromIntegral minVal
    maxV = fromIntegral maxVal
    desc | minVal == maxVal = name ++ " equal to " ++ show' minV
         | otherwise        = name ++ " from interval ["
                                ++ show' minV ++ ", " ++ show' maxV ++ "]"

-- |Function (@'number' minVal maxVal@) parses natural number from
-- the interval [@minVal@, @maxVal@].
number :: Int -> Int -> Parsec String st Int
number = genericNumber digit readDec show "number"

-- |Parses escape sequence.
escapeSeq :: Parsec String st Char
escapeSeq = char '\\' *> (p_char <|> p_num <|> p_any) <?> "escape sequence"
  where
    -- Parses single-character escape sequence (from table @chars@).
    p_char = lookup' chars <$> (oneOf . fst . unzip $ chars)
    -- Parses numeric escape sequence. Numeric escape sequence starts
    -- with @x@ or @u@.
    p_num =   char 'x' *> p_hexCode 2
          <|> char 'u' *> p_hexCode 4
    p_hexCode n =   (toEnum . fst . head . readHex <$> count n hexDigit)
                <|> (between (char '{') (char '}')
                             (toEnum <$> genericNumber hexDigit readHex
                                (flip showHex "") "character code"
                                (ord minBound) (ord maxBound)))
    -- Parses other escape sequences.
    p_any = anyChar
    chars = zip escapeCodes escapeChars

-- |Skips all spaces and comments.
skipSpacesAndComments :: Parsec String st ()
skipSpacesAndComments = skipMany (space <|> comment)
  where
    space = oneOf whiteChars >> return () <?> "space"
    comment = do _ <- char '#'
                 skipMany (noneOf "\n\r")
                 (eof <|> eol)
            <?> "comment"
    eol =   (char '\r' >> option () (char '\n' >> return ()))
        <|> (char '\n' >> option () (char '\r' >> return ()))
        <?> "end of line"

-- |Same as @'number'@ function but skips all spaces and comments after
-- the number.
number_ :: Int -> Int -> Parsec String st Int
number_ minVal maxVal = number minVal maxVal <* skipSpacesAndComments

-- |Same as @'escapeSeq'@ function but skips all spaces and comments after
-- the escape sequence.
escapeSeq_ :: Parsec String st Char
escapeSeq_ = escapeSeq <* skipSpacesAndComments

-- |Same as @'char'@ function but skips all spaces and comments after the read
-- character.
char_ :: Char -> Parsec String st Char
char_ c = char c <* skipSpacesAndComments

-- |Same as @'noneOf'@ function but skips all spaces and comments after
-- the read character.
noneOf_ :: String -> Parsec String st Char
noneOf_ cs = noneOf cs <* skipSpacesAndComments

