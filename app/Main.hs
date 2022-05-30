module Main where

main :: IO ()
main = putStrLn $ show test

-- Homework Exercises
-- -------------------------------------------------------------------

-- Exercise 1
-- -------------------------------------------------------------------
-- We need to find the digits of a number. Define the
-- functions
--     toDigits    :: Integer -> [Integer]
--     toDigitsRev :: Integer -> [Integer]
--
-- toDigits should convert positive Integers to a list of digits.
-- (For 0 or negative inputs, toDigits should return the empty
-- list.)
-- toDigitsRev should do the same, but with the digits reversed.

-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigitsRev 1234 == [4,3,2,1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | x < 10 = [x]
  | otherwise = let truncated = x `div` 10
                 in toDigits truncated ++ [x `rem` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | x < 10 = [x]
  | otherwise = let truncated = x `div` 10
                 in x `rem` 10 : toDigitsRev truncated

-- Exercise 2
-- -------------------------------------------------------------------
-- Once we have the digits in the proper order, we need to double
-- every other one. Define a function
--    doubleEveryOther :: [Integet] -> [Integer]
--
-- Remember that doubleEveryOther should double every other
-- number beginning from the right, that is, the second-to-last,
-- fourth-to-last, ...numbers are doubled.
--
-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  foldr (\x acc -> if shouldDouble acc then double x:acc else x:acc) [] xs
  where
    double = (*2)
    shouldDouble xs
      | even ((+1) . length $ xs) = True
      | otherwise = False

-- Exercise 3
-- -------------------------------------------------------------------
-- The output of doubleEveryOther has a mix of one-digit and
-- two-digit numbers. Define the function
--     sumDigits :: [Integer] -> Integer 
--
-- to calculate the sum of all digits.
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 12 + 5

sumDigits :: [Integer] -> Integer
sumDigits xs
  | null xs        = 0
  | length xs == 1 = calculate (head xs)
  | otherwise      = calculate (head xs) + sumDigits (tail xs)
  where calculate x
          | x `div` 10 == 0 = x
          | otherwise  = let (truncated, remainder) = x `divMod` 10
                          in remainder + calculate truncated 

-- Exercse 4
-- -------------------------------------------------------------------
-- A card number is valid when the remainder of the sum (of the
-- digits) divided by 10 is zero.
--
-- Define the function
--    validate
--
-- that indicates whether an integer could be a valid credit card
-- number. This will use all functions defined in the previous
-- exercise.
--
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False 

validate :: Integer -> Bool
validate x = case (sumDigits . doubleEveryOther . toDigits $ x) `rem` 10 of
              0 -> True
              _ -> False 

-- Test
test :: [Bool]
test = let pass = validate 4012888888881881 -- True 
           fail = validate 4012888888881882 -- False 
        in [pass, fail]

