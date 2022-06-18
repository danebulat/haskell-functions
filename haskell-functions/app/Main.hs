module Main where

import qualified CardValidation as CV
import qualified WordNumber as WN
import Cipher 

import Data.Char (toUpper)

runCipherDemo :: IO ()
runCipherDemo = do
  putStrLn "Enter a sentence to pass to the caesar cipher:"
  x <- getLine
  putStrLn "Number of times to shift the characters to the right:"
  y <- getLine
  print  $ Cipher.caesar (read y :: Int) x

-- -------------------------------------------------------------------
-- Program selector function
-- -------------------------------------------------------------------

runProgram :: Int -> IO ()
runProgram x =
  case x of
    1 -> print CV.test
    2 -> putStrLn $ WN.wordNumber 1234567890
    3 -> runCipherDemo
    _ -> putStrLn "Not recognized"

-- -------------------------------------------------------------------
-- Main function
-- -------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Enter program number:"
  i <- getLine
  runProgram (read i :: Int)
  

-- -------------------------------------------------------------------
-- Utilities (add to module)
-- -------------------------------------------------------------------

-- Exercise
-- --------
-- Write a function that capitalizes sentences in a paragraph.
-- Recognize when a new sentence has behin by checking for
-- periods.

-- 1. Capitalize the first character in the paragraph.
-- 2. Check for a pattern every 3 characters. Move squence
--    along by one character each iteration.
-- 3. When the string only has 1 or 2 characters left, return
--    the remaining string as-is.

capitalizeParagraph :: String -> String
capitalizeParagraph seq =
  let h = toUpper (head seq)
      s = h : (tail seq)
   in go s
   where go [] = []
         go (x:[]) = x : go ([])
         go (x:y:[]) = x : y : go ([])
         go s@(x:y:z:xs) =
               if shouldCapitalize s
               then x : y : (toUpper z) : go xs
               else x : go (y:z:xs)

shouldCapitalize seq =
  case (take 2 seq) of
    ". " -> True
    "? " -> True
    "! " -> True
    _    -> False
