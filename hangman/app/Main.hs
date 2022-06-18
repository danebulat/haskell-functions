-- src/Main.hs
module Main where

import HangMan
import Data.Char (toLower)

-- main 
main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle








