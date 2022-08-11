module Main where

import Control.Monad.State

-- ================================================================================
-- Tic Tac Toe Game

-- --------------------------------------------------------------------------------
-- Data types

data Slot =
  Empty | Naught | Cross deriving (Eq, Show)

data Grid =
  Grid [Slot] deriving (Eq)


-- --------------------------------------------------------------------------------
-- Show instance

instance Show Grid where
  show (Grid slots) = drawHorz ++ go slots
    where
      -- draw horizontal line 
      drawHorz = "\n" ++ (replicate 13 '-') ++ "\n"

      -- draw each row followed by a horizontal line
      go [] = []
      go (x:y:z:zs) = drawRow [x,y,z] ++ drawHorz ++ go zs

      -- functions to draw a row
      drawRow slots = "|" ++ go slots
        where   
          go []     = []
          go (x:xs) = drawChar (drawSlot x) ++ "|" ++ go xs

          drawSlot Empty  = ' '
          drawSlot Naught = 'o'
          drawSlot Cross  = 'x'

          drawChar c = [' ', c, ' ']


-- --------------------------------------------------------------------------------
-- Grid generators

mkEmptyGrid :: Grid
mkEmptyGrid = Grid slots
  where slots = replicate 9 Empty

mkWinningGrid :: Grid
mkWinningGrid = Grid slots
  where slots = [ Cross,  Cross,  Cross,
                  Naught, Empty,  Naught,
                  Empty,  Naught, Empty ]


-- --------------------------------------------------------------------------------
-- Utilities

-- Return a new state with the updated grid but
-- the same slot.
--
applyMove :: Int -> State (Grid, Slot) ()
applyMove target = do
  ((Grid slots), slot) <- get
  let newGrid = go 0 target slots slot
  put (Grid newGrid, slot)
  where
    go _ _ [] _ = []
    go index target (x:xs) s
      | index == target = s : xs
      | otherwise       = x : go (index + 1) target xs s

-- Check if there's a winner on the grid and set
-- the state's Bool value appropriately.
--
checkWinner :: State (Grid, Slot) Bool
checkWinner = do
  (Grid slots, currentSlot) <- get
  case checkWin slots currentSlot of
    True  -> pure True
    False -> pure False
  
  where
    -- checks each winning combination with the grid. The foldr
    -- produces [Slot, Slot, Slot] where [Cross, Cross, Cross] or
    -- [Naught, Naught, Naught] is a win for that Slot.
    checkWin slots currentSlot =
      let getBools =
            map (\xs -> let rs = foldr (\i acc -> slots !! i : acc) [] xs
                        in all (== currentSlot) rs
                ) winningCombos
      in True `elem` getBools
    
    -- winning combinations represented as grid indices.
    winningCombos = [ [0, 1, 2]
                    , [3, 4, 5]
                    , [6, 7, 8]
                    , [0, 3, 6]
                    , [1, 4, 7]
                    , [2, 5, 8]
                    , [0, 4, 8]
                    , [6, 4, 2] ]

-- Swap the state's Slot value to represent the next
-- turn.
--
changeSlot :: State (Grid, Slot) ()
changeSlot = do
  (grid, slot) <- get
  case slot of
    Naught -> put (grid, Cross)
    _      -> put (grid, Naught)


-- --------------------------------------------------------------------------------
-- Running the game

-- [0,5,1,6,2] (win for Cross if Cross goes first)
-- State ~ (Grid, Slot) where Slot represents the current turn (Naught of Cross)
--
runTicTacToe :: [Int] -> State (Grid, Slot) Bool
runTicTacToe [] = checkWinner >>= pure
runTicTacToe (move:moves) = do
  applyMove move
  isWinner <- checkWinner
  
  if isWinner
    then pure isWinner
    else do
      changeSlot  -- change turn (next slot)
      runTicTacToe moves

-- Run the game and present the final grid and
-- who won the game (Naught or Cross)
--
testTicTacToe :: IO ()
testTicTacToe = do
  let (r, (grid, slot)) =
        -- pass a list of ints only including numbers 0 to 8, Ie:
        -- [0, 5, 1, 6, 2]
        -- [1, 3, 6, 4, 8, 5, 2, 7]
        runState (runTicTacToe [1, 3, 6, 4, 8, 5, 2, 7]) (mkEmptyGrid, Naught) -- Naught goes first
  case r of
    -- there was a winner
    True -> do
      putStrLn $ "\nThe winner was: " ++ show slot
      putStrLn $ show grid

    -- there was no winner
    False -> do
      putStrLn $ "\nNo winner!\n"
      putStrLn $ show grid


-- --------------------------------------------------------------------------------
-- Main

main :: IO ()
main = testTicTacToe
