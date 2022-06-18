# The `Maybe` and `Either` types

```haskell
data Maybe a = Nothing | Just a 
```

- Lets us return a default `Nothing` value when we don't have any 
  sensible values to return for our intended type `a`.
  
  ```haskell 
  ifEvenAdd2 :: Integral a => a -> Maybe a 
  ifEvenAdd2 n 
    | even n = Just n 
    | otherwise = Nothing  
  ```

## Smart constructors for datatypes 

```haskell 
type Name = String 
type Age = Integer 

data Person = Person Name Age deriving Show 

mkPerson :: Name -> Age -> Maybe Person 
mkPerson name age 
  | name /= "" && age >= 0 = 
      Just $ Person name age 
  | otherwise = Nothing
```

- `mkPerson` is what we call a **smart constructor**. It allows us 
  to construct values of a type only when they meet certain criteria,
  so that we know we have a valid value, and return an explicit 
  signal when we do not.

## Either type 

```haskell
data Either a b = Left a | Right b
```

An example on how to use separate validation functions and return 
a possible list of errors.

```haskell 

-- Constants representing Person errors
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- Alias representing a list of person errors, or a 
-- valid value. 
type ValidatePerson a = Either [PersonInvalid] a

-- Separate validation functions 
ageOkay :: Age -> ValidatePerson Age 
ageOkay age
  | age >= 0 = Right age 
  | otherwise = Left [AgeToolow]
  
nameOkay :: Name -> ValidatePerson Name
nameOkay name 
  | name /= "" = Right name 
  | otherwise = Left [NameEmpty]
  
-- Function to construct a Person that returns an Either value
mkPerson :: Name -> Age -> ValidatePerson Person 
mkPerson name age = 
  mkPerson' (nameOkay name) (ageOkay age)
  
-- Utility function that matches the results of the separate 
-- validation functions, and returns an Either value.
mkPerson' :: Either ValidatePerson Name 
          -> Either ValidatePerson Age 
          -> Either ValidatePerson Person 
mkPerson' (Right n) (Right a) = Right $ Person n a
mkPerson' (Left en) (Left ea) = Left $ en ++ ea
mkPerson' (left en) _         = Left en
mkPerson' _         (Left ea) = Left ae
```

## Kinds

- Type constructors (that is, higher-kinded types) are types that take 
  more types as arguments. 
  
- The Haskell report uses the term __type constants__ to refer to types 
  that take no arguments and are already types. In the report, __type 
  constructor__ is used to refer to types which must have arguments 
  applied to become a type.

- The `Maybe` and `Either` datatypes also have type constructors rather 
  than constants. They have to be applied to an argument before they 
  become concrete types. 
  
- Confirm with `:kind` in GHCi.

### Lifted and unlifted types 

- To be precise, kind `*` is the kind of all standard lifted types, 
  while types that have the kind `#` are unlifted.
  
- A lifted type, which includes any datatype you could define yourself,
  is any that can be inhabited by bottom. Lifted types are represented 
  by a pointer and include most of the datatypes we've seen and most 
  that you're likely to encounter and use.
  
- Unlifted types are any type which cannot be inhabited by bottom. 
  Types of kind `#` are often native machine types and raw pointers.
  
- Newtypes are a special case in that they are kind `*`, but are 
  unlifted because their representation is identical to that of the 
  type they contain, so the newtype itself is not creating any new 
  pointer beyond that of the type it contains.
  
- That fact means that the newtype itself cannot be inhabited by 
  bottom, only the thing it contains can be, so newtypes are unlifted.
  
- The default kind of concrete, fully-applied datatypes in GHC is 
  kind `*`.
  
- The type `Maybe` is a __type constructor__ because it takes one 
  argument before it becomes a concrete type.

## Data constructors are functions 

- Data constructors that haven't been fully applied have function 
  arrows in them. Once you apply them to their arguments, they return 
  a value of the appropriate item. In other words, data constructors 
  are functions. They behave like Haskell functions in that they are 
  curried as well.
  
- Data constructors that take arguments do behave like functions.
  Like functions, their arguments are typechecked against the 
  specification in the type.
  
## Useful functions 

```haskell 
:t iterate
iterate :: (a -> a) -> a -> [a]

take 10 $ iterate (+1) 0
[0,1,2,3,4,5,6,7,8,9]

:t unfoldr 
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- The `b` is the starting value in the 
-- iteration.

-- Maybe (a, b)
-- 'a' is returned as the current value 
-- 'b' will be the next value 

take 10 $ unfoldr (\b -> Just (b, b+1)) 0
[0,1,2,3,4,5,6,7,8,9]
```

Implementing `iterate` and `unfoldr`:

```haskell 
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Nothing -> []
  Just (x, y) -> x : myUnfoldr f y
  
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))
```
