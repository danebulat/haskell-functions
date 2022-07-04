module Exercises where

import Control.Monad
import System.Random ( randomRIO )

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes 

import Prelude hiding ( Left, Right )

-- -------------------------------------------------------------------
-- (>>=) :: Monad m
--       -> m a -> (a -> m b) -> m b
--
-- Write Monad instances for the following types. Use the
-- QuickCheck properties to validate the instances.

-- -------------------------------------------------------------------
-- 1.
data Nope a =
  NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

nopeTests :: IO ()
nopeTests = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  where trigger :: Nope (Int, Char, Int)
        trigger = undefined

-- -------------------------------------------------------------------
-- 2.

data PtEither b a =
  Left a | Right b
  deriving (Eq, Show)

instance Functor (PtEither b) where
  fmap f (Left a) = Left (f a)
  fmap _ (Right b) = Right b

instance Applicative (PtEither b) where
  pure = Left
  (Left f)  <*> (Left a)  = Left (f a)
  (Left _)  <*> (Right b) = Right b
  (Right b) <*> _         = Right b

instance Monad (PtEither b) where
  return = pure
  (Left a)  >>= k = k a
  (Right b) >>= _ = Right b

instance (Arbitrary b, Arbitrary a)
      => Arbitrary (PtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Left b)
              , (1, return $ Right a) ]

instance (Eq b, Eq a)
      => EqProp (PtEither b a) where
  (=-=) = eq

runEitherTests :: IO ()
runEitherTests = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  where trigger :: PtEither (Int, Char, String) (Int, Char, String)
        trigger = undefined 

-- -------------------------------------------------------------------
-- 3. Write a Monad instance for Identity 

newtype Identity a =
  Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= k = k a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

runIdentityTests :: IO ()
runIdentityTests = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  where trigger :: Identity (Int, Char, String)
        trigger = undefined 

-- -------------------------------------------------------------------
-- 4.

data List a =
  Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Nil = Nil

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs =
    fmap f xs `append'` (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= k =
    k x `append'` (xs >>= k) -- k :: a -> List a

append' :: List a -> List a -> List a
append' Nil ys = ys
append' (Cons x xs) ys = Cons x (append' xs ys)

foldList :: [a] -> List a
foldList = foldr Cons Nil

getRandom :: IO Int
getRandom = randomRIO (1, 10)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Cons a (Cons b Nil))

instance Eq a => EqProp (List a) where
  (=-=) = eq

runListTest :: IO ()
runListTests = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  where trigger :: List (Int, Char, String)
        trigger = undefined

-- -------------------------------------------------------------------
-- Run all tests

runAllTests :: IO ()
runAllTests = do
  putStrLn "\nTesting: Nope a"
  nopeTests
  
  putStrLn "\nTesting: PtEither b a"
  runEitherTests
  
  putStrLn "\nTesting: Identity a"
  runIdentityTests
  
  putStrLn "\nTesting: List a"
  runListTests 

-- -------------------------------------------------------------------
-- Write the following functions using the methods provided by
-- Monad and Functor. Using stuff like identity and composition
-- is fine, but it has to typecheck with types provided.

-- 1.
j :: Monad m => m (m a) -> m a
j m = join m

tests1 = let r1 = j [[1, 2], [], [3]]
             r2 = j (Just (Just 1))
             r3 = j (Just Nothing)  -- Nothing 
             r4 = j Nothing         -- Nothing
          in r1 == [1, 2, 3]
          && r2 == Just 1

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = liftM f m

tests2 :: Bool
tests2 = (l1 show (Just 2)) == Just "2"
             
-- 3.
l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = liftM2 f m1 m2

tests3 :: Bool
tests3 = (l2 (+) (Just 1) (Just 2)) == (Just 3)

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a m1 m2 = m2 <*> m1

-- 5.
-- You'll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = sequence $ go xs f
  where go :: [a] -> (a -> m b) -> [m b]
        go [] _ = []
        go (x:xs) aToMb = [aToMb x] ++ (go xs aToMb)        

-- 6.
-- Hint: reuse "meh"
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = sequence xs
