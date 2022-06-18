module Main where

import Test.Hspec
import HangMan
--import Test.QuickCheck

incFn :: Integer -> Integer
incFn x = x + 1

-- -------------------------------------------------------------------
-- HangMan sample data 
-- -------------------------------------------------------------------

samplePuzzle :: Puzzle
samplePuzzle =
  Puzzle "elephant"
         [Just 'e', Nothing, Just 'e', Nothing, Nothing, Nothing, Just 'n', Nothing]
         "en"

samplePuzzle' :: Puzzle
samplePuzzle' =
  Puzzle "elephant"
         [Just 'e', Nothing, Just 'e', Nothing, Nothing, Just 'a', Just 'n', Nothing]
         "aen"

sampleFreshPuzzle :: Puzzle
sampleFreshPuzzle =
  Puzzle "dolphin" (go 7) ""
  where go :: Integer -> [Maybe Char]
        go 0 = []
        go n = Nothing : go (n-1)

-- -------------------------------------------------------------------
-- Main
-- -------------------------------------------------------------------

main :: IO ()
main = hspec $ do

  describe "Arithmetic" $ do
    it "x + 1 is greater than x" $ do
      incFn 1 `shouldBe` 2

  describe "HangMan" $ do
    it "max and min word length correct" $ do
      minWordLength < maxWordLength `shouldBe` True

    it "fillInCharacter fills in one correct character" $ do
      (fillInCharacter samplePuzzle 'a') `shouldBe` samplePuzzle'

    it "creates a fresh puzzle" $ do
      freshPuzzle "dolphin" `shouldBe` sampleFreshPuzzle

    it "handles an already guessed character" $ do
      (alreadyGuessed samplePuzzle 'e') `shouldBe` True

    it "handles a not-yet guessed characer" $ do
      (alreadyGuessed samplePuzzle 't') `shouldBe` False
  

