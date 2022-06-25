module Main where 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes ( applicative, monoid )

import Data.Monoid
import Control.Applicative

--import Ch17Exercises ( List, Pair, Two, Three, Three' )
import Exercises ( Pair, Two, Three, Three' )
import Lists ( List )
import qualified Lists as Z

-- -------------------------------------------------------------------
instance Semigroup a
      => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)
  -- call (<>) on inner values 

instance Monoid a
      => Monoid (ZipList a) where
  mempty = ZipList []

--instance Eq a
--      => EqProp (ZipList a) where
--  (=-=) = eq

--instance Arbitrary a
--      => Arbitrary (ZipList a) where
--  arbitrary = ZipList <$> arbitrary 

--instance Arbitrary a
--      => Arbitrary (Sum a) where
--  arbitrary = Sum <$> arbitrary
-- -------------------------------------------------------------------

-- Control.Applicative.ZipList Monoid
runZipListMonoidTest :: IO ()
runZipListMonoidTest =
  let zl = ZipList [1 :: Sum Int]
  in quickBatch $ monoid zl

-- List Applicative
runListApplicativeTest :: IO ()
runListApplicativeTest =
  quickBatch . applicative $ trigger
  where trigger :: List (Int, [Char], Integer)
        trigger = undefined 

-- ZipList' Applicative
runZipListApplicativeTest :: IO ()
runZipListApplicativeTest =
  quickBatch . applicative $ trigger
  where trigger :: Z.ZipList' (Int, Int, Int)
        trigger = undefined 

-- Pair
qcPair :: IO ()
qcPair = quickBatch . applicative $ trigger
  where trigger :: Pair (Int, Char, Integer)
        trigger = undefined

-- Two
qcTwo :: IO ()
qcTwo = quickBatch . applicative $ trigger
  where trigger :: Two ([Int], String, Sum Int) (Int, Char, Integer)
        trigger = undefined

-- Three
qcThree :: IO ()
qcThree = quickBatch . applicative $ trigger
  where trigger :: Three ([Int], Sum Int, String) ([Int], String, Sum Int) (Int, Char, Integer)
        trigger = undefined

-- Three'
qcThree' :: IO ()
qcThree' = quickBatch . applicative $ trigger
  where trigger :: Three' (Sum Int, Product Int, [Int]) (Int, Char, String)
        trigger = undefined

-- Main 
main :: IO ()
main = runListApplicativeTest






