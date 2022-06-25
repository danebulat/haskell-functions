module Exercises where

import Data.Functor.Const
import Data.Monoid
import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import qualified Data.Validation as V

-- -------------------------------------------------------------------
-- Either vs Validation 
-- -------------------------------------------------------------------

eitherExamples :: ()
eitherExamples =
  let r1 = pure 1 :: Either e Int             -- Right 1
      r2 = Right (+1) <*> Right 1             -- Right 2
      r3 = Right (+1) <*> Left ":("           -- Left ":("
      r4 = Left ":("  <*> Right 1             -- Left ":("
      r5 = Left ":("  <*> Left "sadface.png"  -- Left ":("
   in ()

-- How does Validation differ? Principally in what the Applicative
-- instance does with errors. Rather than just short-circuiting when
-- it has two error values, it'll use the Monoid typeclass to combine
-- them.
--
-- Often this'll just be a list of errors, but you can do whatever
-- you want.

data Errors =
    DivideByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show) 

success :: V.Validation [String] Int
success = V.Success (+1)
      <*> V.Success 1

validationTest1 :: Bool
validationTest1 =
  success == V.Success 2

failure :: V.Validation [Errors] Int
failure = V.Success (+1)
      <*> V.Failure [StackOverflow]

validationTest2 :: Bool
validationTest2 =
  failure == V.Failure [StackOverflow]

failure' :: V.Validation [Errors] Int
failure' = V.Failure [StackOverflow]
       <*> V.Success (+1)

validationTest3 :: Bool
validationTest3 =
  failure' == V.Failure [StackOverflow]

failures :: V.Validation [Errors] Int
failures =
      V.Failure [MooglesChewedWires]
  <*> V.Failure [StackOverflow]

validationTest4 :: Bool
validationTest4 =
  failures == V.Failure  [MooglesChewedWires
                        , StackOverflow]

-- Exercises: Variations on Either
-- -------------------------------------------------------------------

data MyValidation e a =
    MyFailure e
  | MySuccess a
  deriving (Eq, Show)

-- same as Either
instance Functor (MyValidation e) where
  fmap _ (MyFailure e) = MyFailure e
  fmap f (MySuccess a) = MySuccess $ f a 

-- This is different
instance Monoid e =>
         Applicative (MyValidation e) where
  pure a = MySuccess a
  (MySuccess f) <*> (MySuccess x)  = MySuccess (f x)
  (MyFailure e) <*> (MyFailure e') = MyFailure (e <> e')
  (MySuccess _) <*> (MyFailure e)  = MyFailure e
  (MyFailure e) <*> (MySuccess _)  = MyFailure e

-- ===================================================================
-- 17.9 Chapter Exercises (p.1135)
-- ===================================================================

-- Given a type that has an instance of Applicative, specialize the
-- types of the methods. Test your specialization in the REPL.

-- 1. -- Type
--       []
--
--    -- Methods
--    pure :: a -> [] a
--    (<*> :: [] (a -> b) -> [] a -> [] b

-- 2. -- Type
--       IO
--
--    -- Methods
--    pure :: a -> IO a
--    (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. -- Type:
--       (,) a
--
--    -- Methods
--    pure :: b -> (,) a b
--    (<*>) :: (,) a (b -> c) -> (,) a b -> (,) a c

-- 4. -- Types:
--       (->) e
--
--    -- Methods:
--    pure :: a -> (->) e a
--    (<*>) :: (->) e (a -> b) -> (->) e a -> (->) e b

-- -------------------------------------------------------------------
-- Write instances for the following datatypes. Use the checkers
-- library to validate the instances.

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') =
    Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- -------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a f) <*> (Two b c) = Two (a <> b) (f c)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b 

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- -------------------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) =>
         Applicative (Three a b)  where
  pure x = Three mempty mempty x
  (Three a b f) <*> (Three c d e) =
    Three (a<>c) (b<>d) (f e)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-------------------------------------------------------------------

data Three' a b
  = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f f') <*> (Three' a' b b') =
    Three' (a<>a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where 
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- -------------------------------------------------------------------
-- Remember the voewls and stops exercises in the folds chapter?
-- Write the function to generate the possible combinations of three
-- input lists using liftA3 from Control.Applicative.

stops :: String 
stops = "pbtdkg"

vowels :: String 
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos xs ys zs = liftA3 (\x y z -> (x,y,z)) xs ys zs

