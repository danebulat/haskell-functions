# Morse Project 

## Hspec and QuickCheck Notes

- Introductory Hspec and QuickCheck examples.

- Various arithmetic functions used to demonstrate tests including 
  `recSum` (summation), `dividedBy`, 
  
- Generating a specific set of values for a QuickCheck random 
  generator. Can test with QuickCheck `sample` and `sample'` 
  functions.
  
  ```haskell 
  oneThroughThree :: Gen Int 
  oneThroughThree = elements [1..3]
  
  oneThroughThree' :: Gen Int 
  oneThroughThree' = elements [1,2,2,2,2,3]
  
  genOrdering :: Gen Ordering
  genOrdering = elements [LT, EQ, GT]
  
  genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
  genEither = do 
    a <- arbitrary 
    b <- arbitrary 
    elements [Left a, Right b]
    
  genMaybe :: Arbitrary a => Maybe a 
  genMaybe = do
    a <- arbitrary
    frequency [ (1,  return Nothing)
              , (10, return (Just a)) ]

  -- oneof 
  sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b) 
  sumGenEqual = do 
    a <- arbitrary 
    b <- arbitrary 
    oneof [ return $ First a
          , return $ Second b ]
          
  sumGenEqualCharInt :: Gen (Sum Char Int)
  sumGenEqualCharInt = sumGenEqual
  ```

## `tests/WordNumberTest.hs`

### Hspec examples 

- Testing functions with expected values:

  ```haskell 
  describe "digitToWord" $ do 
    it "returns zero for 0" $ do 
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do 
      digitToWord 1 `shouldBe` "one"
  ```

### QuickCheck examples 

- Testing functions with QuickCheck:

  ```haskell 
  describe "ch14Exercises" $ do 
    it "halfidentity x gives x" $ do 
      property $ \x -> halfIdentity x == (x :: Double)
  ```

- Sort function implementation with tests:

  ```haskell 
  listOrdered :: (Ord a) => [a] -> Bool 
  listOrdered xs = 
    snd $ foldr go (Nothing, True) = status 
    where go _ status@(_, False) = status 
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, _)       = (Just y, x >= y)
  ```

- Generating simple random lists and tuples with elements 
  implementing the `Arbitrary` typeclass.
  
  ```haskell 
  threeTupGen :: (Arbitrary a) => Gen (a, a, a)
  threeTupGen = do 
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (x, y, z)
    
  threeTupGenInt :: Gen (Int, Int, Int)
  threeTupGenInt = threeTupGen 
  ```
  
- Solutions for **Chapter 14 Exercises** mostly using QuickCheck.

  ```haskell
  prop_additionAssocCommut :: Property 
  prop_additionAssoccommut = 
    forAll threeTupGenInt (\(x, y, z) -> x + (y + z) == (x + y) + z
                                      && x + y == y + x)
  ```

- **Idempotence** example. 
  
  - Idempotence refers to a property of some functions in which
    the result value does not change beyond the initial application.
    If you apply the function once, it returns a result, and applying
    the same function to that value won’t ever change it.

  - You might think of a list that you sort: once you sort it, the sorted
    list will remain the same after applying the same sorting function to
    it.

  - `QuickCheck` and the helper functions used to demonstrate idempotence 
    for certain functions.
    
- Examples of making **random generators** for arbitrary datatypes.

## `test/tests.hs`

### QuickCheck examples 

- Making instances of `Arbitrary` where we can implement or provide 
  a function to `arbitrary` function for the instance.
  
  ```haskell
  arbitrary :: Arbitrary a => Gen a
  ```

- Generators for arbitrary product and sum types.

  ```haskell 
  data Pair a b 
    = Pair a b deriving (Eq, Show)

  -- generator function 
  pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
  pairGen = do 
    a <- arbitrary 
    b <- arbitrary
    return $ Pair a b
  
  -- Arbitrary instance 
  instance (Arbitrary a, Arbitrary b) 
            => Arbitrary (Pair a b) where 
    arbitrary = pairGen 
    
  -- another generator 
  pairGenIntChar :: Gen (Pair Int Char)
  pairGenIntChar = pairGen 
  ```

- Choosing a different weighting or probabilities than an equal 
  distribution.

  ```haskell 
  sumGenMoreFirst :: (Arbitrary a, Arbitrary a) => Gen (Sum a b)
  sumGenMorefirst = do 
    a <- arbitrary
    b <- arbitrary
    frequency [ (10, return $ First a)
              , (1,  return $ Second b)]
  ```

- `CoArbitrary` mentioned. **TODO:** Read in QuickCheck manual.










