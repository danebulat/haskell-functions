module Main where

import qualified CardValidation as CV
import qualified WordNumber as WN 

main :: IO ()
main = do
  putStrLn "Enter program number:"
  i <- getLine
  runProgram (read i :: Int)

runProgram :: Int -> IO ()
runProgram x =
  case x of
    1 -> print CV.test
    2 -> putStrLn $ WN.wordNumber 1234567890
    _ -> putStrLn "Not recognized"
