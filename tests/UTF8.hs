import Test.HUnit

import Core.Partition
import Data.Word (Word8)
import Core.UTF8
import Data.List (sort)

main :: IO ()
main = do runTestTT suite
          return ()

t_splitRange = "splitRange" ~: test
  [
  -- 1
    splitRange (Range '\0' '\10') ~=? ([Range '\0' '\10'], [], [], [])
  , splitRange (Range '\40' '\127') ~=? ([Range '\40' '\127'], [], [], [])
  -- 1, 2
  , splitRange (Range '\20' '\128') ~=?
    ([Range '\20' '\127'], [Range '\128' '\128'], [], [])
  , splitRange (Range '\0' '\2047') ~=?
    ([Range '\0' '\127'], [Range '\128' '\2047'], [], [])
  -- 1, 2, 3
  , splitRange (Range '\127' '\2048') ~=?
    ([Range '\127' '\127'], [Range '\128' '\2047']
    ,[Range '\2048' '\2048'], [])
  , splitRange (Range '\63' '\55055') ~=?
    ([Range '\63' '\127'], [Range '\128' '\2047']
    ,[Range '\2048' '\55055'], [])
  , splitRange (Range '\1' '\65535') ~=?
    ([Range '\1' '\127'], [Range '\128' '\2047']
    ,[Range '\2048' '\65535'], [])
  -- 1, 2, 3, 4
  , splitRange (Range '\15' '\65536') ~=?
    ([Range '\15' '\127'], [Range '\128' '\2047']
    ,[Range '\2048' '\65535'], [Range '\65536' '\65536'])
  , splitRange (Range '\6' '\822522') ~=?
    ([Range '\6' '\127'], [Range '\128' '\2047']
    ,[Range '\2048' '\65535'], [Range '\65536' '\822522'])
  , splitRange (Range '\127' '\1100000') ~=?
    ([Range '\127' '\127'], [Range '\128' '\2047']
    ,[Range '\2048' '\65535'], [Range '\65536' '\1100000'])
  , splitRange (Range '\0' maxBound) ~=?
    ([Range '\0' '\127'], [Range '\128' '\2047']
    ,[Range '\2048' '\65535'], [Range '\65536' '\1114111'])
  -- 2
  , splitRange (Range '\128' '\128') ~=?
    ([], [Range '\128' '\128'], [], [])
  , splitRange (Range '\128' '\1222') ~=?
    ([], [Range '\128' '\1222'], [], [])
  , splitRange (Range '\147' '\2000') ~=?
    ([], [Range '\147' '\2000'], [], [])
  , splitRange (Range '\128' '\2047') ~=?
    ([], [Range '\128' '\2047'], [], [])
  , splitRange (Range '\1578' '\2047') ~=?
    ([], [Range '\1578' '\2047'], [], [])
  -- 2, 3
  ]

t_charToBytes = "charToBytes 1-4" ~: test
  [
  -- 1
    charToBytes1 '\0' ~=? [0]
  , charToBytes1 '\64' ~=? [64]
  , charToBytes1 '\127' ~=? [127]
  -- 2
  , charToBytes2 '\128' ~=? [0xC2, 0x80]
  , charToBytes2 '\175' ~=? [0xC2, 0xAF]
  , charToBytes2 '\997' ~=? [0xCF, 0xA5]
  , charToBytes2 '\999' ~=? [0xCF, 0xA7]
  , charToBytes2 '\1253' ~=? [0xD3, 0xA5]
  , charToBytes2 '\1987' ~=? [0xDF, 0x83]
  , charToBytes2 '\2040' ~=? [0xDF, 0xB8]
  , charToBytes2 '\2047' ~=? [0xDF, 0xBF]
  -- 3
  , charToBytes3 '\2048' ~=? [0xE0, 0xA0, 0x80]
  , charToBytes3 '\2049' ~=? [0xE0, 0xA0, 0x81]
  , charToBytes3 '\7052' ~=? [0xE1, 0xAE, 0x8C]
  , charToBytes3 '\16384' ~=? [0xE4, 0x80, 0x80]
  , charToBytes3 '\25467' ~=? [0xE6, 0x8D, 0xBB]
  , charToBytes3 '\32767' ~=? [0xE7, 0xBF, 0xBF]
  , charToBytes3 '\32768' ~=? [0xE8, 0x80, 0x80]
  , charToBytes3 '\59001' ~=? [0xEE, 0x99, 0xB9]
  , charToBytes3 '\65535' ~=? [0xEF, 0xBF, 0xBF]
  -- 4
  , charToBytes4 '\65536' ~=? [0xF0, 0x90, 0x80, 0x80]
  , charToBytes4 '\175211' ~=? [0xF0, 0xAA, 0xB1, 0xAB]
  , charToBytes4 '\512301' ~=? [0xF1, 0xBD, 0x84, 0xAD]
  , charToBytes4 '\1014667' ~=? [0xF3, 0xB7, 0xAE, 0x8B]
  , charToBytes4 maxBound ~=? [0xF4, 0x8F, 0xBF, 0xBF]
  ]

t_bytesToChar = "bytesToChar" ~: test
  [
  -- bytesToChar is inverse of charToBytes 1-4.
    let chars = ['\0'..'\127']
    in map (bytesToChar . charToBytes1) chars ~=? chars
  , let chars = ['\128'..'\2047']
    in map (bytesToChar . charToBytes2) chars ~=? chars
  , let chars = ['\2048', '\2049', '\20543', '\31484', '\32768', '\45876'
                ,'\55456', '\65535']
    in map (bytesToChar . charToBytes3) chars ~=? chars
  , let chars = ['\65536', '\65799', '\94234', '\175841', '\546448'
                ,'\745576', '\957416', '\1046036', maxBound]
    in map (bytesToChar . charToBytes4) chars ~=? chars
  ]

t_convertRange = "t_convertRange" ~: test
  [
    testRange '\0' '\0'
  , testRange '\0' '\33'
  , testRange '\65' '\65'
  , testRange '\65' '\66'
  , testRange '\0' '\128'
  , testRange '\65535' '\65536'
  , testRange '\547' '\1248'
  , testRange '\0' '\2048'
  , testRange '\1999' '\2508'
  , testRange '\32475' '\33000'
  , testRange '\48253' '\48255'
  , testRange '\65500' '\65800'
  , testRange '\1114011' '\1114111'
  , testRange '\1114111' maxBound
  ]
  where
    testRange lo hi = rangeToChars lo hi ~=? [lo..hi]

    rangeToChars lo hi = sort $ map bytesToChar $ concatMap sequenceToBytes
                                                $ convertRange (Range lo hi)
      where
        sequenceToBytes :: Sequence -> [[Word8]]
        sequenceToBytes [] = [[]]
        sequenceToBytes (Range a b:ranges)
          = concat [map (w:) (sequenceToBytes ranges) | w <- [a..b]]

suite = test
  [
    t_splitRange, t_charToBytes, t_bytesToChar, t_convertRange
  ]
