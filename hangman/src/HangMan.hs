module HangMan where

import Control.Monad (forever)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type alias
--type WordList = [String]

newtype WordList
  = WordList [String] deriving (Eq, Show)

-- word length bounds
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- get all words from dict.txt 
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

-- filter words to fit min and max word length
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
           in l >= minWordLength && l < maxWordLength

-- pull random word from list
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, upper)
  return $ wl !! randomIndex
  where upper = (length wl) - 1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- -------------------------------------------------------------------
-- making a puzzle
-- -------------------------------------------------------------------

data Puzzle =
  Puzzle { getWord :: String, getFound :: [Maybe Char], getGuesses ::  [Char] }
  deriving (Eq)
  --                    1                       2                         3
  -- 1) The word we're trying to guess
  -- 2) The character we've filled in so far
  -- 3) the letters we've guessed so far 

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
      " Guessed so far: " ++ guessed

-- Take our puzzle word and turn it into a list
-- of Nothing (1st step in hiding word from player)
--
-- Inputs:  The word to guess for this round
-- Outputs: Puzzle instance with hidden representation of word

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (mkHiddenWord w) []
  where mkHiddenWord [] = []
        mkHiddenWord (x:xs) = Nothing : mkHiddenWord xs

-- Now we need a function that looks at the Puzzle String and
-- determines whether the character you guessed is an element
-- of that string.

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) x = x `elem` word
  
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) x = x `elem` guessed

-- Function to render the partially guessed word.
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar Nothing  = '_'

-- Function to fill in a character if its guessed correctly.
type GuessedChar  = Char 
type WordChar     = Char
type FilledInChar = Maybe Char

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c:s)
  where
    zipper :: GuessedChar -> WordChar -> FilledInChar -> Maybe Char
    zipper guessed wordChar filledChar =
      if guessed == wordChar  -- If guessed char is same as char in the word
        then Just wordChar    -- Return wrapped character
        else filledChar       -- Otherwise, return Nothing 

    -- Re-construct filledInSoFar
    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar
                      -- [same      length] --

-- Function to handle guesses. Gives different responses based
-- on whether the guessed character:
--
-- > had already been guessed previously;
-- > is in the word and needs to be filled in;
-- > or, was not previously guseed but also isn't in the puzzle word.

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]

  -- case expression to handle game outcomes
  case ( charInWord puzzle guess
       , alreadyGuessed puzzle guess) of

    -- handle already guessed 
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick\
              \ something else!"
      return puzzle

    -- handle correct guess
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)

    -- handle a failed guess
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

-- Game over after a set number of guesses
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guesses) =
  if (countBadGuesses wordToGuess guesses) > 7 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

-- -------------------------------------------------------------------
-- Exercises
-- -------------------------------------------------------------------

countBadGuesses :: [Char] -> [Char] -> Int
countBadGuesses word guesses = go word guesses
  where go _ [] = 0
        go word (x:xs)
          | x `elem` word = (go word xs)
          | otherwise = 1 + (go word xs)

sample :: Puzzle
sample =
  (Puzzle "elephant"
           [Just 'e', Nothing, Just 'e', Nothing, Nothing, Nothing, Nothing, Nothing]
           ['e', 'z', 'w', 'b'])

-- -------------------------------------------------------------------
-- End exercises
-- -------------------------------------------------------------------

-- Handle winning a game
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

-- The instruction for running a game
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                   \ be a single character"
