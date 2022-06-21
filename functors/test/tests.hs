{-# LANGUAGE ViewPatterns #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

-- Functor laws:
-- fmap id = id
-- fmap f . g == (fmap f) . (fmap g)

-- Turning those into the following QuickCheck properties
functorIdentity :: (Functor f, Eq (f a))  -- Functor with Eq instance
                => f a                    -- Functor to fmap id
                -> Bool                   -- Result 
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f)   -- Functor with Eq instance for result functor 
               => (a -> b)                -- 1st function for fmap 
               -> (b -> c)                -- 2nd function fo fmap 
               -> f a                     -- Functor to fmap over
               -> Bool                    -- Result
functorCompose f g x = (fmap (g . f) x) == (fmap g (fmap f x))

-- Making QuickCheck generate functions too (p.1015)
-- -------------------------------------------------------------------
-- {-# LANGUAGE ViewPatterns #-}

-- We pattern match on the `Fun` value that we're
-- asking QuickCheck to generate.

-- The underlying Fun type is essentally a product of the QuickCheck
-- Function type, and an ordinary Haskell function generated from
-- Function.
--
-- We only want the second part, the ordinary Haskell function, so
-- we're pattern matching that one out:

functorCompose' :: (Eq (f c), Functor f) =>  
                   f a                       -- Our functor to fmap over 
                -> Fun a b                   -- First random function to apply 
                -> Fun b c                   -- Second random function to apply 
                -> Bool                      -- Test result 
functorCompose' x (Fun _ f) (Fun _ g) =      -- `f` and `g` are contained random functions in Fun
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Type of random functions `f` and `g` (Int -> Int)
type IntToInt = Fun Int Int 

-- Type of functorCompose'
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool 

-- Function to give QuickCheck
qcf1 = functorCompose' :: IntFC

-- -------------------------------------------------------------------
-- Exercises:
-- Implement Functor instances for the following datatypes. Use the
-- QuickCheck properties to validate them.
-------------------------------------------------------------------

-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- type of function to generate to test functor laws with `Identity a`
type IntToInt' = Fun Int Int
type StringToString = Fun String String 

-- type of functorCompose'
type IdentityIntFC = Identity Int -> IntToInt' -> IntToInt' -> Bool
type IdentityStringFC = Identity String -> StringToString -> StringToString -> Bool

-- test functor laws by generating random functions
qcf2 :: IdentityIntFC
qcf2 = functorCompose'

qcf3 :: IdentityStringFC
qcf3 = functorCompose'

-- -------------------------------------------------------------------
-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a  <- arbitrary
    a' <- arbitrary
    return (Pair a a')

-- type of functorCompose'
type PairIntFC = Pair Int -> IntToInt -> IntToInt -> Bool 

-- apply functorCompose to Pair Int
qcf4 :: PairIntFC
qcf4 = functorCompose'

-- -------------------------------------------------------------------
-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- type of functorCompose'
type TwoIntFC = Two Char Int -> IntToInt -> IntToInt -> Bool

qcf5 :: TwoIntFC
qcf5 = functorCompose'

-- -------------------------------------------------------------------
-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeIntFC = Three Char Char Int
               -> IntToInt -> IntToInt -> Bool

qcf6 :: ThreeIntFC
qcf6 = functorCompose' 

-- -------------------------------------------------------------------
-- 5.
data Three' a b = Three' b a a deriving (Eq, Show)
--                       ^ fmap function applied here 

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' (f a) b b'

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

type ThreeIntFC' = Three' Int Int -> IntToInt -> IntToInt -> Bool

qcf7 :: ThreeIntFC'
qcf7 = functorCompose'

-- -------------------------------------------------------------------
-- 6.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourIntFC = Four Int Char Int Int -> IntToInt -> IntToInt -> Bool 

qcf8 :: FourIntFC
qcf8 = functorCompose'

-- -------------------------------------------------------------------
-- 7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z b) = Four' x y z (f b)

instance (Arbitrary a, Arbitrary b)
         => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

type FourIntFC' = Four Int Char Char Int
               -> IntToInt -> IntToInt -> Bool

qcf9 :: FourIntFC'
qcf9 = functorCompose'

-- -------------------------------------------------------------------
-- Run tests 

main :: IO ()
main = do
  quickCheck qcf1 -- Example 
  
  quickCheck qcf2 -- Identity a
  quickCheck qcf3
  quickCheck (functorIdentity :: Identity Int -> Bool)

  quickCheck qcf4 -- Pair a
  quickCheck (functorIdentity :: Pair Int -> Bool)

  quickCheck qcf5 -- Two a b
  quickCheck (functorIdentity :: Two Char Int -> Bool)

  quickCheck qcf6 -- Three a b c
  quickCheck (functorIdentity :: Three Char Char Int -> Bool)

  quickCheck qcf7 -- Three' a b b
  quickCheck (functorIdentity :: Three' Char Int -> Bool)

  quickCheck qcf8 -- Four a b c d
  quickCheck (functorIdentity :: Four Int Char Int Int -> Bool)

  quickCheck qcf9 -- Four' a b
  quickCheck (functorIdentity :: Four' Char Int -> Bool)
  
  return ()
