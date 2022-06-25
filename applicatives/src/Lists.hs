module Lists where 

import Data.Monoid
import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes 

-- ($)   :: (  a -> b) ->   a ->   b
-- fmap  :: (  a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- ===================================================================
-- List Applicative Exercises
--
-- Implement the list Applicative. Writing a minimally complete
-- Applicative instance calls for writing the definition of
-- both `pure` and `<*>`. Use the `checkers` library to validate
-- your Applicative instance.
-- ===================================================================

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- Functor instance 
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Applicative instance 
instance Applicative List where
  pure a = Cons a Nil 
  _ <*> Nil = Nil 
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = fmap f xs `append'` (fs <*> xs)

-- QuickCheck Instances
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    frequency [ (1, return Nil)
              , (3, return (Cons a Nil))
              , (3, return (Cons a (Cons b Nil)))
              , (3, return (Cons a (Cons b (Cons c Nil))))
              ]

-- Checkers Instances
instance Eq a => EqProp (List a) where
  (=-=) = eq

-- ===================================================================
-- ZipList Applicative Exercise
-- 
-- Implement the ZipList Applicative. Use the checkers library to
-- validate your applicative instance.
-- ===================================================================

-- ZipList' (List a)
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

-- Checkers instances 
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take'' 3000 l
          ys' = let (ZipList' l) = ys
                in take'' 3000 l

-- QuickCheck
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    frequency [ (1, return $ ZipList' (Cons a Nil))
              , (1, return $ ZipList' (Cons a (Cons b Nil)))
              , (1, return $ ZipList' (Cons a (Cons b (Cons c Nil))))
              ]

-- ZipList' functor 
instance Functor ZipList' where 
  fmap _ (ZipList' Nil) = ZipList' Nil
  fmap f (ZipList' xs)  = ZipList' $ fmap f xs
    -- use (List a) implementation of fmap 

-- ZipList' applicative 
instance Applicative ZipList' where 
  pure a = ZipList' $ Cons a Nil 

  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil

  ZipList' fs <*> ZipList' xs =
    ZipList' $ go fs xs
    where go Nil _ = Nil
          go _ Nil = Nil
          go (Cons f fs) (Cons x xs) =
            Cons (f x) (go fs xs)

-- ===================================================================
-- Utility functions 
-- ===================================================================

-- Takes a normal function and a (List a).
-- Returns a List of those values applied to the function (fmap)

apply' :: (a -> b) -> List a -> List b
apply' f (Cons x xs) = Cons (f x) (apply' f xs)
apply' _ Nil = Nil

-- Append implementation
-- Joins two (List a) together
--
append' :: List a -> List a -> List a
append' Nil ys = ys
append' (Cons x xs) ys =
  Cons x (append' xs ys)

-- Take implementation (Safe)
--
take' :: Int -> List a -> Maybe (List a)
take' n xs
  | n <= 0    = Nothing
  | otherwise = Just (go n xs)
  where go _ Nil = Nil 
        go 0 _   = Nil
        go n (Cons x xs) = Cons x (go (pred n) xs)

-- Take implementation (Non-safe)
-- 
take'' :: Int -> List a -> List a
take'' 0 _ = Nil
take'' n Nil = Nil 
take'' n (Cons x xs) = Cons x (take'' (n-1) xs)

-- Quickly builds a list with foldr 
--
buildList :: [a] -> List a
buildList = foldr Cons Nil

-- flatMap puts a Cons element between every other
-- Cons element in the list.
--
fn :: Num a => a -> List a
fn x = x `Cons` (9 `Cons` Nil)

flatMap :: (a -> List b) -> List a -> List b
flatMap f (Cons x xs) = f x `append'` flatMap f xs
flatMap _ Nil = Nil

flatMapTest :: Bool 
flatMapTest =
  let r1 = flatMap fn (buildList [1..3])
   in r1 == Cons 1 (Cons 9 (Cons 2 (Cons 9 (Cons 3 (Cons 9 Nil)))))
                                           
-- Another way to manipulate a (List a) structure.
-- Provide a custom function for manipuling the Cons values.
--

foldList :: (a -> b -> b) -> b -> List a -> b
foldList _ acc Nil = acc
foldList f acc (Cons x xs) = f x (foldList f acc xs)

testFoldList :: Bool 
testFoldList =
  let r1 = foldList (\x acc -> Cons (x+1) acc) Nil (Cons 1 (Cons 2 Nil))
   in r1 == Cons 2 (Cons 3 Nil)

-- ===================================================================
-- Experimental (learning) functions 
-- ===================================================================


-- Builds a (List a) structure where the
-- lowest integer is the head of the list.
--
buildOrderedList :: Int -> List Int
buildOrderedList = reverseList . buildIntList

-- Builds a (List a) structure where highest
-- integer is the head of the list.
--
buildIntList :: Int -> List Int 
buildIntList n
  | n == 0 = Nil
  | otherwise = Cons n (buildIntList (n-1))

-- Reverses a (List a) structure without
-- using the built-in [] type.
-- 
reverseList :: List a -> List a
reverseList list =
  let len = getListLength list
  in go list len
  where
    go :: List a -> Int -> List a
    go list len
      | len == 0 = Nil
      | otherwise = Cons (getListElement (len-1) list) (go list (len-1))

-- Returns the length of a (List a) structure
-- as an Int.
--
getListLength :: List a -> Int
getListLength Nil = 0
getListLength (Cons x xs) = 1 + getListLength xs

-- Takes an index and returns the corresponding
-- element in a (List a) structure.
--
getListElement :: Int -> List a -> a
getListElement targetIn list =
  let len = getListLength list
  in if len >= (targetIn+1) then go targetIn list 0 else go (len-1) list 0
  where
    go targetIn l curIn
          | targetIn == curIn =
            let (Cons x xs) = l in x
          | otherwise = let (Cons x xs) = l
                         in go targetIn xs (1+curIn)

