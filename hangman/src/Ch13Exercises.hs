
module Ch13Exercises where

import System.Exit (exitSuccess)
import Control.Monad (forever)

-- 3.
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

-- 4.
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show 

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =  Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknown $
                              "Name was: " ++ show name ++
                              "Age was: "  ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Input name: "
  name <- getLine
  putStr "Input age: "
  age <- getLine
  validatePerson name (read age :: Integer)
    where
      validatePerson :: String -> Integer -> IO ()
      validatePerson name age =
        case (mkPerson name age) of
          Left (PersonInvalidUnknown msg) ->
            putStrLn $ "Error: " ++ msg
          Left NameEmpty -> 
            putStrLn "Error: Name was empty"
          Left AgeTooLow -> 
            putStrLn "Error: Age too low"
          Right p -> do
            putStrLn "Yay! Successfully got a person: "
            print p
                
-- Fibonacci
-- -------------------------------------------------------------------

fib :: Integer -> Integer
fib n
  | n <= 1 = n
  | otherwise = fib (n-1) + fib (n-2)


fib' :: Integer -> Integer
fib' n = go 1 0 n 0
  where go f1 f2 n c
          | c <= n = go (f1+f2) f1 n (c+1)
          | otherwise = f2




