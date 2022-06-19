-- test/tests.hs
module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck
--import Test.QuickCheck.Gen (oneof)

type Morse = String

-- -------------------------------------------------------------------
-- Set up generators for ensuring that the random
-- values QuickCheck uses to test our program
-- are sensible for our Morse code program.

-- allowed characters 
allowedChars :: [Char]
allowedChars = M.keys letterToMorse

-- allowed Morse 
allowedMorse :: [Morse]
allowedMorse = M.keys morseToLetter

-- generators 
charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

-- -------------------------------------------------------------------
-- Morse tests 
-- -------------------------------------------------------------------

-- Now we write up the property we want to check. We
-- want to check that when we convert something to Morse
-- code and then back again, it comes out the same string
-- we started out with.

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

-- ===================================================================
-- 14.6 Arbitrary instances (p.863)
-- ===================================================================

-- -------------------------------------------------------------------
-- Babby's First Arbitrary
-- -------------------------------------------------------------------

data Trivial =
  Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-- -------------------------------------------------------------------
-- Identity Crisis
-- -------------------------------------------------------------------

newtype Identity a =
  Identity a deriving (Eq, Show)

-- We need `a` to also implement Arbitrary 
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

-- We can make identityGen the default generator for
-- the Identity type by making it the `arbitrary`
-- value in the `Arbitrary` instance.
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

-- An applied generator for Identity for generating
-- Ints. 
identityGenInt :: Gen (Identity Int) -- <--
identityGenInt = identityGen

-- Can change the concrete type of Identity's type
-- arguments and generate values of different types.
ex7 :: IO [Identity Integer]
ex7 = sample' (identityGen :: Gen (Identity Integer))

ex7' :: IO [(Identity Int)]
ex7' = sample' identityGenInt

-- -------------------------------------------------------------------
-- Arbitrary Products
-- -------------------------------------------------------------------

data Pair a b =
  Pair a b deriving (Eq, Show)

-- Creating a generator for a product type
pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Pair a b
  
-- Making Pair an instance of arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

-- An applied Pair generator for generating random
-- Ints and Char 
pairGenIntChar :: Gen (Pair Int Char)
pairGenIntChar = pairGen

-- -------------------------------------------------------------------
-- Greater than the sum of its parts 
-- -------------------------------------------------------------------

-- Writing `Arbitrary` instances for sum types requires the following
-- import (now redundant):
--
-- import Test.QuickCheck.Gen (oneof)

-- Sum types represent disjunction, so with a sum type like `Sum`, we
-- need to represent the exclusive possibilities in our `Gen`. One
-- way to do that is to pull out as many `arbitrary` values as you
-- require for the cases of your sum type.
--
-- We have two data constructors in this sum type, so we'll want two
-- `arbitrary` values. Then we'll repack them into `Gen` values,
-- resulting in a value of type `[Gen a]` that can be passed to `oneof`.

data Sum a b =
  First a | Second b deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ First a,
          return $ Second b ]

-- appied sumGenEqual
sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- -------------------------------------------------------------------
-- You can choose a different weighting of probabilities than an
-- equal distribution. Ie. Snippet of `Maybe` `Arbitrary` instance:

--    instance Arbitrary a => Arbitrary (Maybe a) where
--      arbitrary =
--        frequency [(1, return Nothing),
--                   (3, liftM Just arbitrary)]
--
-- Three times more likely to generate a `Just` value than a
-- `Nothing` value.
-- -------------------------------------------------------------------

-- Implement Arbitrary typeclass for Sum
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGenFirstPls

-- Apply as default arbitrary implementation for Sum 
sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [ (10, return $ First a)
            , (1,  return $ Second b) ]

-- Specialised arbitrary function 
sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

ex8 :: IO [Sum Char Int]
ex8 = sample' sumGenCharIntFirst

-- -------------------------------------------------------------------
-- CoArbitrary
-- -------------------------------------------------------------------

-- `CoArbitrary` is a counterpart to `Arbitrary` that enables the
-- generation of functions fitting a particular type. Rather than
-- talking about random values you can get via `Gen`, it lets you
-- provide functions with a value of type `a` as an argument in
-- order to vary a `Gen`.

-- arbitrary :: Arbitrary a => Gen a
--
-- coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b
--
-- CoArbitrary.hs

main :: IO ()
main = sample trivialGen

-- main :: IO ()
-- main = quickCheck prop_thereAndBackAgain

