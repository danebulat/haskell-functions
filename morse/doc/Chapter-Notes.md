# Chapter Notes 

## Hspec 

```haskell 
shouldBe :: (Show a, Eq a) => a -> a -> Expectation 
--                ^ to show result of test

-- contrast with 
(==) :: (Eq a) => a -> a -> Bool 
```

- Hspec manual: https://hspec.github.io

- __(p.824)__ Initialise simple stack project with `.cabal` file:

  ```bash
  stack init    -- initialize stack file describing stackage snapshot
  stack build   -- build program, pull dependencies
  stack ghci 
  ```

## QuickCheck 

- QuickCheck was the first library to offer "property testing".
- Hspec is more for unit testing.
- QuickCheck is a dependency of Hspec.
- Hspec has QuickCheck integration out of the box.

- QuickCheck tests many values to see if your assertions hold for
  all of them.
  - Randomly generates values of the type you expect.
  - Runs 100 tests by default.
  
- Relies on `Arbitrary` typeclass and `Gen` newtype for generating 
  its random data.
  
  ```haskell 
  arbitrary :: Arbitrary a => Gen a 
  ```

  - `arbitrary` returns type of `Gen a`.
  - We provide a type of which we want random values.
  
- Can use `sample` and `sample'` from the `Test.QuickCheck` module
  in order to see some random data.
  
  ```haskell 
  sample  :: Show a => Gen a -> IO ()
  sample' :: Gen a -> IO [a]
  ```

- We use the `Arbitrary` typeclass in order to provide a generator 
  for `sample`.
- `Gen` is a newtype with a single type argument. It exists for wrapping 
  up a function to generate pseudorandom values.

  ```haskell 
  sample (arbitrary :: Gen Integer)
  ```

- Running `sample arbitrary` directly in GHCi without specifying a type 
  defaults in returning `()` values. 
  
  ```haskell 
  -- in GHCi 
  sample arbitrary 
  ```

- Note:

  ```haskell 
  :t return 
  return :: Monad m => a -> m a 
  
  -- When m is Gen: 
  return :: a -> Gen a
  
  -- Gen wraps a type implementing the Arbitrary typeclass
  arbitrary :: Arbitrary a => Gen a 
  ```

### A way to set a default generator for a type 

- When you use `arbitrary`, you have to specify the type to dispatch 
  the right typeclass instance (as type and typeclass instances 
  form unique pairings).
  
- To see random data, we can use `sample` and `sample' from
  `Test.QuickCheck` module:
  
  ```haskell 
  -- prints each value on a new line
  sample  :: Show a => Gen a -> IO ()
  
  -- returns a list
  sample' :: Gen a -> IO [a]
  ```

- The `IO` is necessary because it's using a global resource of 
  random values to generate data. 
   
  - Pulls from a global resource of random values; something pure 
    functions cannot do.
    
- We use the `Arbitrary` typeclass to provide a generator for `sample`.

  - `Arbitrary` is unprincipled, meaning it has no laws and nothing it's
     supposed to do.
     
  - Convenient way of creating a `Gen a` generator for type `a`.
  
- `Gen` newtype exists for wrapping up a function to generate pseudorandom 
  values. 
  
  ```haskell 
  -- Example 1: Passing an Int random generator 
  sample (arbitrary :: Gen Int)
  
  -- Example 2: Passing a Double random generator 
  sample (arbitrary :: Gen Double)
  ```

- Use `:info Arbitrary` in GHCi to see what instances are available.

## Using QuickCheck without Hspec 

```haskell 
prop_additionGreater :: Int -> Bool 
prop_additionGreater x = x + 1 > x 

runQc :: IO ()
runQc = quickCheck prop_additionGreater
```
