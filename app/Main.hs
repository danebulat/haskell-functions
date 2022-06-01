module Main where

import qualified CardValidation as CV
import qualified WordNumber as WN
import Cipher 

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
  
