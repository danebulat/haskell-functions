module KleisliComposition where

import Control.Monad ((>=>))

--   (>=>)
--     :: Monad m
--     => (a -> m b) -> (b -> m c) -> a -> mc

--   flip (.)
--     :: (a -> b)   -> (b -> c)   -> a -> c

-- It's function composition with monadic structure hanging off
-- functions we're composing.

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine 

readM :: Read a => String -> IO a
readM = return . read 

-- We need the Kleisli composition operator to stitch
-- sayHi and readM together:

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

