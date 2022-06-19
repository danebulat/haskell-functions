module WordNumberTest where

import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)
import Data.List (sort)
import Data.Char (toUpper)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

  describe "ch14Exercises" $ do
    it "halfIdentity gives x" $ do
      property $ \x -> halfIdentity x == (x :: Double)

-- -------------------------------------------------------------------
-- Using QuickCheck 
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- 1.
-- -------------------------------------------------------------------

-- for a function
half :: Fractional a => a -> a
half x = x / 2

-- this property should hold
halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

-- make a generator of doubles to feed the test
doubleGen :: Gen Double
doubleGen = arbitrary

-- property to test halfIdentity function with random doubles 
prop_halfIdentity :: Property
prop_halfIdentity =
  forAll doubleGen (\x -> x == halfIdentity x)

-- -------------------------------------------------------------------
-- 2.
-- -------------------------------------------------------------------

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, _)       = (Just y, x >= y)

-- create a list generator 
listGen :: (Arbitrary a, Ord a) => Gen [a]
listGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return [a, b, c, d]

-- apply integer to list generator 
integerListGen :: Gen [Integer]
integerListGen = listGen

-- quickcheck property 
prop_listOrdered :: Property
prop_listOrdered =
  forAll integerListGen (listOrdered . sort)
                        --(\x -> listOrdered (sort x))

-- -------------------------------------------------------------------
-- 3.
-- -------------------------------------------------------------------

-- Now we'll test the associative and commutative properties of
-- addition.
--
-- plusAssociative x y z =  x + (y + z) == (x + y) + z
-- plusCommutative x y = x + y == y + x 
--
-- Keep in mind that these properties won't hold for types based on
-- IEEE-754 floating point numbers, such as Float or Double.

-- create an three item tuple generator
threeTupGen :: (Arbitrary a) => Gen (a, a, a)
threeTupGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (x, y, z)

-- apply integer to three item tuple 
threeTupGenInt :: Gen (Int, Int, Int)
threeTupGenInt = threeTupGen

-- property
prop_additionAssocCommut :: Property
prop_additionAssocCommut =
  forAll threeTupGenInt (\(x, y, z) -> x + (y + z) == (x + y) + z
                                    && x + y == x + y )

-- -------------------------------------------------------------------
-- 4. 
-- -------------------------------------------------------------------

-- -- Now do the same for multiplication.
prop_multiplicationAssocCommut :: Property
prop_multiplicationAssocCommut =
  forAll threeTupGenInt (\(x, y, z) -> x * (y * z) == (x * y) * z
                                    && x * y == y * x)

-- -------------------------------------------------------------------
-- 5.
-- -------------------------------------------------------------------

-- There are some laws involving the relationship of `quot` and `rem`
-- and `div` and `mod`. Write `QuickCheck` tests to prove them.
--
-- quot rem
-- (quot x y)*y + (rem x y) == x
-- (div  x y)*y + (mod x y) == x

-- two item tuple generator
twoTuple :: Arbitrary a => Gen (a, a)
twoTuple = do
  x <- arbitrary
  y <- arbitrary
  return (x, y)

-- avoids dividing by negative numbers and zero
twoTupleInt :: Gen (Int, Int)
twoTupleInt = do
  (x, y) <- twoTuple
  return (1 + abs x ,1 + abs y)

prop_integralDivision :: Property
prop_integralDivision =
  forAll twoTupleInt (\(x, y) ->  (quot x y)*y + rem x y == x
                              &&  (div  x y)*y + rem x y == x)

-- -------------------------------------------------------------------
-- 6.
-- -------------------------------------------------------------------

-- Is (^) associative? Is it commutative? Use Quickcheck to see
-- if the computer can contradict such as assertion.

-- Generator non-zero
threeTupleGenIntPos :: Gen (Int, Int, Int)
threeTupleGenIntPos = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (1+abs x, 1+abs y, 1+abs z)

-- Associativity property 
prop_powerOfAssoc :: Property
prop_powerOfAssoc =
  forAll threeTupleGenIntPos (\(x, y, z) -> (x ^ y) ^ z == x ^ (y ^ z))

-- Commutative property
prop_powerOfComm :: Property
prop_powerOfComm =
  forAll twoTupleInt (\(x, y) -> x ^ y == y ^ x)

-- -------------------------------------------------------------------
-- 7.
-- -------------------------------------------------------------------

-- Test that reversing a list twice is the same as the identity
-- of the list
--
-- reverse . reverse == id

prop_revRevListIsIdentity :: Property
prop_revRevListIsIdentity =
  forAll integerListGen (\x -> (reverse . reverse $ x) == id x)

-- -------------------------------------------------------------------
-- 8.
-- -------------------------------------------------------------------

-- Write a property for the definition of ($)
--
-- f $ a = f a
-- f . g = \x -> f (g x)

intGen :: Gen Int
intGen = arbitrary

prop_dollarFn :: Property
prop_dollarFn = forAll intGen (\x -> ((*2) $ x) == (*2) x)

prop_compFn :: Property
prop_compFn = forAll intGen (\x -> let y = (*2) . (+3) $ x
                                       z = (*2) ((+3) x)
                                    in y == z)

-- -------------------------------------------------------------------
-- 9.
-- -------------------------------------------------------------------

-- See if these two functions are equal:
--
-- foldr (:) == (++)
-- foldl (++) [] == concat

prop_foldrConsIsAppend :: Property
prop_foldrConsIsAppend =
  forAll integerListGen (\x -> let r1 = foldr (:) [] x
                                   r2 = foldr (++) [] [x]
                                in r1 == r2)

nestedIntListGen :: Gen [[Integer]]
nestedIntListGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return [[a,b], [c, d]]

prop_foldlAppendIsConcat :: Property
prop_foldlAppendIsConcat =
  forAll nestedIntListGen (\x -> let r1 = foldl (++) [] x
                                     r2 = concat x
                                  in r1 == r2)

-- -------------------------------------------------------------------
-- 10.
-- -------------------------------------------------------------------

-- Hm. Is that so?
--
-- f n xs = length (take n xs) == n

f :: Int -> [a] -> Bool
f n xs = length (take n xs) == n

genIntListInt :: Gen ([Int], Int)
genIntListInt = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return ([a, b, c], d)

prop_fTest :: Property
prop_fTest = forAll integerListGen (\x -> f (length x) x)

-- -------------------------------------------------------------------
-- 11.
-- -------------------------------------------------------------------

f2 :: (Eq a, Read a, Show a) => a -> Bool
f2 x = (read (show x)) == x

prop_f2ShowRead :: Property
prop_f2ShowRead = forAll intGen (\x -> f2 x)

-- Failure
-- Find out why this property fails.
--
-- for a function
square :: Num a => a -> a
square x = x * x

-- why does this property not hold?
-- Examine the type of sqrt.
squareIdentity :: Floating a => a -> a
squareIdentity = sqrt . square

floatGen :: Gen Float
floatGen = arbitrary

prop_squareSqrtIdentity :: Property
prop_squareSqrtIdentity = forAll floatGen (\x -> x == squareIdentity x)

-- -------------------------------------------------------------------
-- Idempotence
-- -------------------------------------------------------------------

-- Idempotence refers to a property of some functions in which
-- the result value does not change beyond the initial application.
-- If you apply the function once, it returns a result, and applying
-- the same function to that value won’t ever change it.

-- You might think of a list that you sort: once you sort it, the sorted
-- list will remain the same after applying the same sorting function to
-- it.

-- Use `QuickCheck` and the following helper functions to demonstrate
-- idempotence for the following:

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

-- ----
-- 1.
-- ----

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

f3 x = (capitalizeWord x == twice capitalizeWord x)
    && (capitalizeWord x == fourTimes capitalizeWord x)

stringGen :: Gen String
stringGen = arbitrary

prop_capitalizeWordIdempotence :: Property
prop_capitalizeWordIdempotence = forAll stringGen f3

-- ----
-- 2.
-- ----

f4 x = (sort x == twice sort x)
    && (sort x == fourTimes sort x)

prop_sortIdempotence :: Property
prop_sortIdempotence = forAll stringGen f4

-- -------------------------------------------------------------------
-- Make a Gen random generator for the datatype 
-- -------------------------------------------------------------------

-- ----
-- 1.
-- ----
-- Equal probabiities for each

data Fool =
  Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

-- ----
-- 2.
-- ----
-- 2/3s chance of Fulse, 1/3 chance of Frue
genMoreFulse :: Gen Fool
genMoreFulse = frequency [ (3, return Fulse)
                         , (1, return Frue) ]

-- -------------------------------------------------------------------
-- QuickCheck main
-- -------------------------------------------------------------------

qcMain :: IO ()
qcMain = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_additionAssocCommut
  quickCheck prop_multiplicationAssocCommut
  quickCheck prop_integralDivision

  quickCheck prop_powerOfAssoc       -- fail 
  quickCheck prop_powerOfComm        -- fail
  quickCheck prop_revRevListIsIdentity

  quickCheck prop_dollarFn
  quickCheck prop_compFn

  quickCheck prop_foldrConsIsAppend
  quickCheck prop_foldlAppendIsConcat

  quickCheck prop_fTest
  quickCheck prop_squareSqrtIdentity -- fail

  quickCheck prop_capitalizeWordIdempotence
  quickCheck prop_sortIdempotence


