module Main where

import Control.Monad
import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- -------------------------------------------------------------------
-- Exercise: Write `bind` in terms of `fmap` and `join`.
--
-- join :: Monad m => m (m a) -> m a
--
-- fmap :: Functor f => (a -> b) -> f a -> f b
--
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b 

-- Keep in mind this is (>>=) flipped 
bind' :: Monad m => (a -> m b) -> m a -> m b
bind' fn m = join (fmap fn m)
            -- fn <<= m

bindTest :: Bool
bindTest =
  let r = bind' (\x -> [x, 1]) [3, 4, 5]
  in r == [3, 1, 4, 1, 5, 1]

-- Monads also lifts
-- -------------------------------------------------------------------

-- liftA and liftM are just like fmap with a different
-- typeclass constraint.
--
liftEx :: Bool 
liftEx =
  let r1 = liftA show (Just 12)
      r2 = liftM show (Just 12)
   in r1 == r2

lift2Ex :: Bool
lift2Ex =
  let r1 = liftA2 (,) (Just 3) (Just 5)
      r2 = liftM2 (,) (Just 3) (Just 5)
      r3 = zipWith (+) [3, 4] [5, 6]  -- [8, 10]
      r4 = liftM2  (+) [3, 4] [5, 6]  -- [8, 9, 9, 10]
      r5 = liftA2  (+) [3, 4] [5, 6]  -- [8, 9, 9, 10]
   in r1 == r2 && r3 == r4

-- ===================================================================
-- 18.3 Do syntax and monads (p.1154)
-- ===================================================================

-- We can see what `do` syntax looks like after the compiler
-- desugars it for us by manually transforming it ourselves:

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

-- We can do the same with the variable binding that `do`
-- syntax includes:

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name 

binding' :: IO ()
binding' =
  getLine >>= putStrLn

-- When fmap alone isn't enough
-- -------------------------------------------------------------------

notEnough :: IO (IO ())
notEnough = putStrLn <$> getLine 

-- fmap :: Functor f => (a -> b) -> f a -> f b
--
-- fmap :: (String -> IO ()) -> IO String -> IO (IO ())
--         (a -> b)             f a          fb
--
-- The IO action returned by putStrLn is not evaluated.

howToFix :: IO () 
howToFix = join $ putStrLn <$> getLine 

-- `join` merged the effects of `getLine` and `putStrLn` into a single
-- IO action. It performs the effects in the order determined by the
-- nesting of the IO actions.

-- Ex:
-- Waiting to evaluate IO actions
printOne :: IO () 
printOne = putStrLn "1"

printTwo :: IO ()
printTwo = putStrLn "2"

twoActions :: (IO (), IO ())
twoActions = (printOne, printTwo)

evaluatingActions :: IO () 
evaluatingActions =
  fst twoActions >> snd twoActions

-- Back to desugaring `do` syntax with our now-enriched understanding
-- of what monads do for us:

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>= \name ->
                putStrLn ("y helo thar: " ++ name)

-- As the nesting intensifies, you can see how `do` syntax can make
-- things a bit cleaner and easier to read:

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn "age pls:" >>
  getLine >>=
  \age -> putStrLn ("y helo thar: "
                    ++ name ++ " who is: "
                    ++ age ++ " years old.")

-- ===================================================================
-- 18.4 Examples of Monad use (p.1163)
-- ===================================================================

-- List Monad 
-- -------------------------------------------------------------------

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: [a] -> (a -> [b]) -> m b

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs           -- bind individual values out of list (like list comprehension)
  if even x
    then [x*x, x*x] -- generates more values, increasing size of list 
    else [x*x]

-- [1, 4, 4, 9]
ex1 :: [Integer]
ex1 = twiceWhenEven [1..3]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-- [4, 4]
ex2 :: [Integer]
ex2 = twiceWhenEven' [1..3]

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs =
  xs >>= \x -> if even x then [x*x, x*x] else [x*x]

-- Maybe Monad
-- -------------------------------------------------------------------

-- src/CowExample.hs

-- Either Monad
-- -------------------------------------------------------------------

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> (Second b) = Second (f b)
  (Second _) <*> (First a)  = First a
  (First a)  <*> _          = First a

instance Monad (Sum a) where
  return = pure
  (Second b) >>= k = (k b)
  (First a) >>= k = First a

-- ===================================================================
-- 18.5 Monad laws 
-- ===================================================================

-- Identity laws
-- -------------------------------------------------------------------
--
-- Means `return` should be neutral and not perform any computation.
-- `return` is only there to put things into structure when we need
-- to, and the existence of structure should not effect the computation.
--
--     right identity:  m >>= return   =  m
--
--     left identity:   return x >>= f = f x

-- Associativity
-- -------------------------------------------------------------------
--
-- Regrouping the functions should not have any impact on the final
-- result.
--
-- When we reassociate them, we need to apply `f` so that `g` has
-- an input value of type `m a` to start the whole thing off. We
-- we pass the argument `x` via an anonymous function.
--
--     (m >>= f) >>= g   =   m >>= (\x -> f x >>= g)

-- QuickCheck monad
-- -------------------------------------------------------------------

runMonadTest :: IO ()
runMonadTest = quickBatch $ monad trigger
  where trigger :: [(Int, Int, Int)]
        trigger = undefined

-- BadMonad.hs

-- ===================================================================
-- 18.6 Application and composition (p.1199)
-- ===================================================================

mcomp :: Monad m => (b -> m c)   -- f
                 -> (a -> m b)   -- g
                 -> (a -> m c)
mcomp f g a = join $ f <$> (g a)

-- But using `join` and `fmap` together means we can use >>= instead:

mcomp' :: Monad m => (b -> m c)  -- f
                  -> (a -> m b)  -- g
                  -> (a -> m c)
mcomp' f g a = g a >>= f 

-- KleisliComposition.hs
-- Demonstration of (>=>) function:
-- 
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
