module WordNumber where 

import Data.List (intersperse)

-- -------------------------------------------------------------------
-- Exercise
-- -------------------------------------------------------------------
-- Fill in the implementations of the functions above so that
-- wordNumber returns the English word version of the Int value.
--
-- You will first write a function that turns integers from 0-9
-- into their corresponding English words, "one", "two", and so
-- on.
--
-- Then you will write a function that takes an integer, separates
-- the digits, and returns it as a list of integers.
--
-- Finally, you will need to apply the first function to the list
-- produced by the second function and turn it into a single
-- string with interspersed hyphens.
--
-- Example:
-- wordNumber 1234546
-- "one-two-thee-four-five-four-six"
-- -------------------------------------------------------------------

digitToWord :: Int -> String 
digitToWord x = case x of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ ->  "error"

digits :: Int -> [Int]
digits x
  | x < 10 = [x]
  | otherwise = let (d, m) = x `divMod` 10
              in digits d ++ [m]


wordNumber :: Int -> [Char]
wordNumber x = concat . intersperse "-" . map digitToWord $ digits x

wordNumber' :: Int -> [Char]
wordNumber' x = r4
  where r1 = digits x
        r2 = map digitToWord r1
        r3 = intersperse "-" r2
        r4 = concat r3

