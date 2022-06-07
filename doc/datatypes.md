# Algebraic Datatypes

- Writing your own datatypes can help you leverage some of Haskell's
  most powerful features - pattern matching, type checking, and 
  inference - in a way that makes your code more concise and clear.

## Data declarations review 

- **Nullary constructor:** A data constructor that takes no arguments.

- **Sum type:** The pipe (`|`) denotes what we call a sum type, a 
  type that has more than one data constructor inhabiting it.

- **Product types:** The data constructors in product types have more 
  than one parameter.

## Data and type constructors

- Although the term "constructor" is often used to describe all type 
  constructors and data constructors, we can make a distinction 
  between **constants** and **constructors**.
  
- Type and data constructors that take no arguments are **constants**.
  They can only store a fixed type and amount of data.
  
- A _type constant_ has no arguments, thus nullary.

## Type constructors and kinds

- Kinds are the types of types, or types one level up. We represent
  kinds in Haskell with `*`.
  
- We query the kind signature of a type constructor (not a data 
  constructor) in GHCi with a `:kind` or `:k`.
  
- The kind of `[]` is `* -> *` because it still needs to be applied to 
  a concrete type before it is itself a concrete type. This is what 
  the constructor of "type constructor" is referring to.

## Data constructors and values 

- The behavior of constructors is such that if they don't take any 
  arguments, they behave like (type or value-level) constants.
  If they do take arguments, they act like (type or value-level)
  functions that don't do anything except get applied.

```haskell 
-- Type constant, constant value 
data PugType = PugData

-- The type variable 'a' is a phantom, or "has no witness".
data HuskyType a = HuskyData
```

## What's a type and what's data?

- Types are static and resolve at compile time. Types are known before 
  runtime, whether through explicit declaration or type inference, 
  and that's what makes them static types.
  
- Information about types does not persist through to runtime. Data 
  are what we're working with at runtime.
  
- Types circumscribe (restrict within limits) values and in that way,
  they describe which values are flowing through what parts of your 
  program.
  
```haskell 
-- type constructors --    compile time
--
-- --------------------    phase separation
--
-- data constructors --    runtime 
```

- **Type argument:** Arguments required by a data constructor.

## Data constructor and arities 

- **Arity** refers to the number of arguments a function or 
  constructor takes.
  
- "-ary" is a common suffix used when talking about mathematical 
  arity, such as with nullary, unary, binary, and the like.

- Data constructors that takes one argument are called **unary**. 
  Data constructors that take more than one argument are called 
  **products**.
  
## What makes these datatypes algebraic?

- Algebraic datatypes in Haskell are algebraic because we can 
  describe the patterns of argument structures using two basic 
  operations: **sum** and **product**

- The **cardinality** of a datatype is the number of possible values 
  it defines. That number can be as small as 0 or as large as 
  infinity (for example, numeric datatypes, lists). Knowing how 
  many possible values inhabit a type can help you reason about 
  your program.
  
- The cardinality of `Bool` is `2`.
  The cardinality of `Int8` is `128 + 127 + 1 = 256`.

## Simple datatypes with nullary data constructors 

- Nullary constructors represent one value when reasoning about the 
  cardinality of the types that inhabit.
  
- This is useful because it tells us that whenever we see a nullary 
  data type in the type signature of a function, we only have to 
  reason about one possible value.

## Unary constructors 

- A unary data constructor takes one argument. Instead of your data 
  constructor being a constant, or a known value, the value will be 
  constructed at runtime from the argument we applied to it.
  
- Datatypes that only contain a unary constructor always have the 
  same cardinality as the type they contain.

- For cardinality, this means unary constructors are the identity 
  function.
  
## `newtype`

- The `newtype` keyword is a way to define a type that can only ever 
  have a single unary data constructor.

- Like other datatypes that have a single unary constructor, the 
  cardinality of a newtype is the same as that of the type it 
  contains.

- A newtype cannot be a product type, sum type, or contain nullary 
  constructors, but it has a few advantages over a vanilla data 
  declaration.
 
- One is that it has no runtime overhead, as it reuses the 
  representation of the type it contains. The difference between 
  newtype and the type it contains is gone by the time the compiler
  generates the code.

- One key contrast between a newtype and a type alias is that you 
  can define the typeclass intsances for newtypes that differ from 
  the instances for their underlying type.

### `GeneralizedNewtypeDeriving` Language Extension 

- For user-defined typeclasses, we can use a language extension 
  called `GeneralizedNewtypederiving`. 

- Language extensions, enabled in GHC by the `LANGUAGE` pragma,
  tell the compiler to process input in ways beyond what the 
  standard provides for.
  
- In this case, this extension will tell the compiler to allow our 
  newtype to rely on a typeclass instance for the type it contains.

- Add to top of source file:

```haskell 
{-# LANGUAGE Generalizednewtypederiving #-}
```

## Product types 

- A product type's cardinality is the product of the cardinalities of 
  its inhabitants. Arithmetically, products are the result of 
  multiplication. 
  
- Where a sum type expresses **or**, a product type expresses **and**.

- A product is a way to carry multiple values around in a single data 
  constructor. Any data constructor with two or more type arguments 
  is a product.
  
- Tuples are anonymous products:

```haskell
( , ) :: a -> b -> (a, b)
```

- The reason it's important to understand cardinality is that the 
  cardinality of a datatype roughly equates to how difficult it is to 
  reason about.
  
## Record syntax

- Records in Haskell are product types with additional syntax to 
  provide convenient accessors to fileds within the record.

## Normal form 

- All the existing algebraic rules for products and sums apply in 
  type systems, and that includes the __distributive property__.
  
- How it works in arithmetic:

  ```haskell 
  2 * (3 + 4)
  2 * 7
  14
  
  -- Can be rewritten with multiplication distributed over the 
  -- addition and obtain the same result:
  
  2 * 3 + 2 * 4
  (6) + (8)
  14
  ```

- This is known as __"sum of products"__. In normal arithmetic, the 
  expression is in normal form when it's been reduced to a final 
  result.
  
- However, if you think of the numerals in the above expressions as 
  representations of set cardinality, then the sum of products 
  expression is in normal form, as there is no computation to 
  perform.
  
- The distributive property can be generalized:

  ```haskell 
  a * (b + c) -> a * b + a * c
  ```

- This is true of Haskell's types as well. Product types distribute 
  over sum types.

  ```haskell 
  -- Given the types
  data FlowerType = Gardenia 
                  | Daisy 
                  | Rose
                  | Lilac 
                  
  type Gardener = String 
  
  data Garden = 
    Garden Gardener FlowerType 
    
  -- What is the sum of products normal form of Garden?
  data Garden = 
      Gardenia Gardener 
    | Daisy    Gardener
    | Rose     Gardener 
    | Lilac    Gardener 
                  
  ```

## Constructing and deconstructing values 

- There are essentially two things we can do with a value: we can 
  generate or construct it or we can match on it and consume it.
  
## Accidental bottoms from records 

- Either define the whole record at once or not at all. 
- An Exception will be raised when trying to access a record without 
  a value.
  
- Whenever we have a product that uses record accessors, keep it 
  separate of any sum type that is wrapping it. To do this, split 
  out the product into an independent type with its own type 
  constructor instead of only as an inline data constructor product.
  
  ```haskell 
  -- Do not do this:
  data Automobile = Null 
                  | Car { make  :: String 
                        , model :: String 
                        , year  :: Integer }
                  deriving (Eq, Show)
                  
  -- Split out the record / product 
  data Car = Car { make  :: String 
                 , model :: String 
                 , year  :: Integer }
             deriving (Eq, Show)
             
  -- The Null is still not great, but 
  -- for demonstration purposes 
  data Automobile = Null 
                  | Automobile Car
                  deriving (Eq, Show)
  ```

## Function type is exponential 

- Given a function `a -> b`, we can calculate the inhabitants with 
  the formula `b^a`
  
- So, if `b` and `a` are `Bool`, then `2^2` is how you could express 
  the number of inhabitants in a function `Bool -> Bool`.
  
- Similarly, a function of `Bool` to something of `3` inhabitants would 
  be `3^2`, and thus have `9` possible implementations.
  
  ```haskell 
  a -> b -> c
  (c ^ b) ^ a
  
  -- Can be rewritten as 
  c ^ (b * a)
  ```

- The type of functions (`->`) is, in the algebra of types, the 
  exponential operator.

### Example

Given the data type:

```haskell 
data Quantum = Yes | No | Both deriving (Eq, Show)
```

Reviewing the arithmetic of sums types:

```haskell
f :: Either Quantum Quantum    -- 3 + 3 = 6
```

Reviewing the arithmetic of product types:

```haskell 
f :: (Quantum, Quantum)       -- 3 * 3 = 9
```

Now a function type; each possible unique implementation of the 
function is an inhabitant:

```haskell 
f :: Quantum -> Quantum       -- 3 ^ 3 = 27
```


## Higher-kinded datatypes 

- Kinds are not types until they are fully applied. Only types have
  inhabitants at the term level. 
  
- The kind `* -> *` is waiting for a single `*` before it is fully 
  applied. The kind `* -> * -> *` must be applied twice before it 
  will be a real type.
  
- This is known as a **higher-kind** type. Lists, for example, are 
  higher-kind datatypes in Haskell.
  
- Because types can be generically ploymorphic by taking type 
  arguments, they can be applied at the type level.
  
  ```haskell
  -- identical to (a, b, c, d)
  data Silly a b c d = 
    MkSilly a b c d deriving Show 
    
  :kind Silly 
  Silly -> * -> * -> * -> * -> *
  
  :kind Silly Int 
  Silly :: * -> * -> * -> *
  
  :kind Silly Int String 
  Silly :: * -> * -> *
  
  :kind Silly Int String Bool 
  Silly :: * -> *
  
  :kind Silly Int String Bool String 
  Silly :: *
  
  :kind (,,,)
  (,,,) :: * -> * -> * -> * -> *
  
  :kind (Int, String, Bool, String)
  (Int, String, Bool, String) :: *
  ```

## Lists are polymorphic 

### Infix type and data constructors

- When we give an operator a nonalphanumeric name, it is infix by 
  default.
  
- Any operator that starts with a colon (`:`) must be an infix type
  of data constructor. All infix data constructors must start with 
  a colon.
  
- The type constructor of function, (`->`), is the only infix type 
  constructor that doesn't start with a colon. 
  
- Another exception is that they cannot be `::` as this syntax is
  reserved for type assertion.
  
  ```haskell 
  -- List type, non-infix 
  data List a = Nil | Cons a (List a) 
  --     1  2    3     5   4   6
  ```
  
  1. The List type constructor.
  2. The 𝑎 type parameter to List.
  3. Nil / empty list value, which also marks the end of a list.
  4. A single value of type 𝑎 in the Cons product.
  5. The Cons constructor, product of 𝑎 and List a.
  6. The rest of our list.
