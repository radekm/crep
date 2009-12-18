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
  , skipSpacesAndComments
  , sepBy
  , sepBy1
  , many
  , many1
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
  , (<**>)
  , (<?>)
  ) where

import Numeric (readDec, readHex)

import Text.Parsec hiding ((<|>), many, space, spaces)
import Control.Applicative hiding (optional, empty)
import Control.Monad (when)

import Core.Utils

-- |Function (@'number' minVal maxVal@) parses natural number from
-- the interval [@minVal@, @maxVal@].
number :: Int -> Int -> Parsec String st Int
number minVal maxVal
  = try (do i <- liftA (fst . head . readDec) (many1 digit)
            when (i < fromIntegral minVal || i > fromIntegral maxVal)
              $ unexpected $ show i
            return $ fromInteger i)
    <?> desc
  where
    desc | minVal == maxVal = "number equal to " ++ show minVal
         | otherwise        = "number from interval ["
                                ++ show minVal ++ ", " ++ show maxVal ++ "]"

-- TODO: handle overflow of hexadecimal character code

-- |Parses escape sequence.
escapeSeq :: Parsec String st Char
escapeSeq = char '\\' *> (p_char <|> p_num <|> p_any) <?> "escape sequence"
  where
    -- Parses single-character escape sequence (from table @chars@).
    p_char = lookup' chars <$> (oneOf . fst . unzip $ chars)
    -- Parses numeric escape sequence. Numeric escape sequence starts
    -- with @x@ or @u@.
    p_num = toEnum . fst . head . readHex
              <$> (try (oneOf "xu" >>
                        between (char '{') (char '}') (many1 hexDigit))
                     <|> char 'x' *> count 2 hexDigit
                     <|> char 'u' *> count 4 hexDigit)
    -- Parses other escape sequences.
    p_any = anyChar
    chars = zip escapeCodes escapeChars

-- |Skips all spaces and comments.
skipSpacesAndComments :: Parsec String st ()
skipSpacesAndComments = skipMany (space <|> comment)
  where
    space = oneOf whiteChars >> return ()
    comment = do char '#'
                 skipMany (noneOf "\n\r")
                 (eof <|> eol)
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

