module VigenereCipher where

import Data.Char (chr, toUpper, ord)

-- -------------------------------------------------------------------
-- A Vigenère cipher is another substitution cipher, based on a Caesar
-- cipher, but it uses a series of Caesar ciphers for polyalphabetic
-- substitution. The substitution for each letter in the plaintext
-- is determined by a fixed keyword.

-- So, for example, if you want to encode the message “meet at dawn,”
-- the first step is to pick a keyword that will determine which Caesar
-- cipher to use. We’ll use the keyword “ALLY” here. You repeat the
-- keyword for as many characters as there are in your original message:

-- MEET AT DAWN
-- ALLY AL LYAL

-- Now the number of rightward shifts to make to encode each character
-- is set by the character of the keyword that lines up with it. The ’A’
-- means a shift of 0, so the initial M will remain M. But the ’L’ for
-- our second character sets a rightward shift of 11, so ’E’ becomes ’P’.
-- And so on, so “meet at dawn” encoded with the keyword “ALLY” becomes
-- “MPPR AE OYWY.”
-- -------------------------------------------------------------------

-- Type synonyms 
type Message = [Char]
type Keyword = [Char]

-- 1. Return a string with the keyword layed over the characters
--    in the message.
--
--    Example: getMask "How are you" "Mask"    == "MAS KMA SKM"
--    Example: getMask "Meet at nine" "YELLOW" == "YELLO WY ELLO"

getMask :: Message -> Keyword -> String
getMask msg keyword = go 0 msg keyword
  where go :: Int -> Message -> Keyword -> String
        go _ [] keyword = []
        go index (x:xs) keyword 
          | x == ' '  = x : go index xs keyword                 -- do not increment index
          | otherwise = keyword !! index : go incr xs keyword   -- increment index
          where
            incr = if (index + 1) > (length keyword) - 1
                   then 0
                   else index + 1

-- 2. Convert the "mask" string to integers, where 'A' is 0 and
-- `Z`is 26.
--
-- Note: ord 'A' = 65 (our offset to each character to base 'A' at 0)
-- Note: ord ' ' = 32 (for handling space the character)
-- Note: Algorithm only supports alphabetic keywords.

getDigits :: String -> [Int]
getDigits [] = []
getDigits (x:xs)
  | ord x == 32 = 32 : getDigits xs         -- handle space character 
  | toUpper x `elem` ['A'..'Z'] =
      ((ord (toUpper x)) - 65) : getDigits xs
  | otherwise = ord x : getDigits xs

-- 3.
-- Shift each letter in the original message to the right. The amount to
-- shift is the corresponding digit in the mask.

rightShift :: Message -> [Int] -> String
rightShift [] mask = []
rightShift (x:xs) (y:ys)
  | ord x == 32 = x : rightShift xs ys      -- handle space character
  | otherwise = let xi = ord x
                    xi' = xi + y
                 in chr xi' : rightShift xs ys

-- 4.
-- Compose functions to produce a vigenere cipher text.
-- Takes a message and a keyword.

vigenere :: Message -> Keyword -> String
vigenere msg key = rightShift msg . getDigits . getMask msg $ key

