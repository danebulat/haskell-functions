module Cipher
( caesar
, unCaesar
, caesarTests
, unCaesarTests
, caesarEncodeDecodeTests
) where

import Data.Char

-- ===================================================================
-- Exercise
-- ===================================================================
-- Write a basic Caesar cipher that shifts rightward.
--
-- ord :: Char -> Int
-- chr :: Int -> Char 
--
-- You want your shift to wrap back around to the beginning
-- of the alphabet, so that if you have a rightward shift
-- of 3 from 'z', you end up back at 'c'.
-- ===================================================================

-- -------------------------------------------------------------------
-- Utility Functions
-- -------------------------------------------------------------------

-- Return True if character not uppercase
-- or lowercase letter
isNotAlpha :: Char -> Bool
isNotAlpha x = not $ x `elem` ['A'..'Z'] || x `elem` ['a'..'z']

-- Get upper and lower bound integer representations
-- of lowercase and uppercase alphabet characters
getBounds :: Char -> (Int, Int)
getBounds x
  | isLower x = (122, 97)  -- Lowercase: 97 - 122
  | isUpper x = (90, 65)   -- Uppercase: 65 - 90
  | otherwise = (0, 0)

-- -------------------------------------------------------------------
-- Encode / Decode Functions
-- -------------------------------------------------------------------

-- Caesar function that shifts characters to
-- the right.
caesar :: Int -> [Char] -> [Char]
caesar n xs = map (\x -> let b = getBounds x in shiftRight n x b) xs
  where
    shiftRight :: Int -> Char -> (Int, Int) -> Char
    shiftRight n c (ub, lb)
      | isNotAlpha c = c
      | n == 0       = c
      | ord c == ub  = shiftRight (n-1) (chr lb) (ub, lb)
      | otherwise    = shiftRight (n-1) (succ c) (ub, lb)
                -- (ub, lb) == (upper bound, lower bound)

-- Caesar function that shifts characters to
-- the left.
unCaesar :: Int -> [Char] -> [Char]
unCaesar n xs = map (\x -> let b = getBounds x in shiftLeft n x b) xs
  where
    shiftLeft :: Int -> Char -> (Int, Int) -> Char
    shiftLeft n c (ub, lb)
      | isNotAlpha c = c
      | n == 0       = c
      | ord c == lb  = shiftLeft (n-1) (chr ub) (ub, lb)
      | otherwise    = shiftLeft (n-1) (pred c) (ub, lb)
                -- (ub, lb) == (upper bound, lower bound)

-- -------------------------------------------------------------------
-- Simpler Character Encode / Decode Functions
-- -------------------------------------------------------------------

-- Simple implementation of shiftRight
-- Takes into account lowercase and space
-- character.
shiftRight' :: Integer -> Char -> Char 
shiftRight' n c
  | c == ' '  = ' ' 
  | n == 0    = c
  | c == 'z'  = shiftRight' (n-1) (chr 97)
  | otherwise = shiftRight' (n-1) (succ c)
  
-- Simple implementation of shiftLeft
-- Takes into account lowercase and space
-- character.
shiftLeft' :: Integer -> Char -> Char
shiftLeft' n c
  | c == ' '  = ' '
  | n == 0    = c
  | c == 'a'  = shiftLeft' (n-1) (chr 122)
  | otherwise = shiftLeft' (n-1) (pred c)

-- ===================================================================
-- Tests
-- ===================================================================

-- Tests for caesar function
-- all should return True
caesarTests :: [Bool]
caesarTests =
  [ caesar 3 "abc"            == "def"
  , caesar 3 "abc "           == "def "
  , caesar 3 "ABC"            == "DEF"
  , caesar 3 "ABC "           == "DEF "
  , caesar 3 "xyz"            == "abc"
  , caesar 3 "xyz "           == "abc "
  , caesar 3 "XYZ"            == "ABC"
  , caesar 3 "XYZ "           == "ABC "
  , caesar (26 * 2 + 1) "abc" == "bcd"
  , caesar (26 * 2 + 1) "ABC" == "BCD"
  ]

-- Tests for unCaesar function
-- all should return True 
unCaesarTests :: [Bool]
unCaesarTests =
  [ unCaesar 3 "xyz"            == "uvw"
  , unCaesar 3 "XYZ "           == "UVW "
  , unCaesar 3 "abc"            == "xyz"
  , unCaesar 3 "ABC "           == "XYZ "
  , unCaesar (26 * 2 + 1) "xyz" == "wxy"
  ]

-- Tests for checking if the original
-- encoded message is decoded
caesarEncodeDecodeTests =
  [ (unCaesar 10 . caesar 10 $ "hello")                  == "hello"
  , (unCaesar 10 . caesar 10 $ "a multiword sentence")   == "a multiword sentence"
  , (unCaesar 256 . caesar 256 $ "Hello. How are you!?") == "Hello. How are you!?"
  ]
