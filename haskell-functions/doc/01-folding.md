# Folding 

## Fold Right 

* Right associative, associates to the right.

  Ex. `1 : 2 : 3 : []` gets passed as `1 : (2 : (3 : []))`

  * Where the inner-most parenthesis is evaluated first.

* `foldr` definition:

  ```haskell 
   foldr :: (a -> b -> b) -> b -> [a] -> b
   foldr f z [] = z
   foldr f z (x:xs) = f x (foldr f z xs)
  ```
  * The `z` provides a fallback value for the empty list case and a 
    __second argument to begin our fold with__; often the identity
    for function `f`.

* Folding `(foldr (+) 0 [1, 2, 3])` where `(+)` is strict in both of 
  its arguments, so it forces the next iteration. (__unconditionally 
  strict__ in both its arguments)
  
  ```haskell 
  foldr (+) 0 [1, 2, 3]
  
  -- evaluates as 
  
  (+) 1 ((+) 2 ((+) 3 0))
      1 + 2 + 3
      3 + 3
      6
  ```

* See `foldr` steps:

  ``` haskell 
  scanr (+) 0 [1, 2, 3, 4]
  ```

* Folding happens in two stages - __traversal__ and __folding__. _(p.541)_
* The difference between the right fold and the left fold is association 
  (parenthesization)
  

## Fold Left 

* Simple definition of `foldl`:

  ```haskell
  foldl :: (b -> a -> b) -> b -> [a] -> b
  foldl f acc []     = acc
  foldl f acc (x:xs) = foldl f (f acc x) xs 
  
  -- given the list
  foldl (+) 0 [1 : 2 : 3 : []]
  
  -- foldl associates like this
  ((0 + 1) + 2) + 3
  ```

* `foldl` begins its reduction process by adding `acc` value to the head of 
  the list. 
  
  * `foldr` had added it to the final element of the list first.

* Using `scanr` and `scanl` to see the difference in evaluation:

  ```haskell 
  scanr (+) 0 [1..5]   --  [15, 14, 12, 9, 5, 0]
                       --                     ^ starts here 

  scanl (+) 0 [1..5]   --  [0, 1, 3, 6, 10, 15]
                       --   ^ starts here
  ```

## Associativity and Folding 

* Both folds traverse the spine in the same direction. What's different is the 
  __associativity of the evaluation__.

  ```haskell
  foldr f z [1,2,3]
  1 `f` (foldr f z [2,3])
  1 `f` (2 `f` (foldr f z [3]))
  1 `f` (2 `f` (3 `f` (foldr f z [])))
  1 `f` (2 `f` (3 `f` z))
               --------- gets evaluated first 
  ```

* __Right folds__ have to traverse the list outside-in, but the folding itself 
  starts __from the end of the list__.

* Example: Using an arithmetic operator that isn't associative:

  ```haskell
  foldr (^) 2 [1..3]   -- 1
  foldl (^) 2 [1..3]   -- 64

  -- 1 ^ (2 ^ (3 ^ 2)) = 1
  -- ((2 ^ 1) ^ 2) ^ 3 = 64 
  
  foldr (^) 2 [1..3]
  1 ^ (2 ^ (3 ^ 2))    -- acc is the last RHS operand 
  1 ^ (2 ^ 9)
  1 ^ 512
  1
  
  foldl (^) 2 [1..3]
  ((2 ^ 1) ^ 2) ^ 3    -- acc starts as the LHS operand 
  (2 ^ 2) ^ 3
  4 ^ 3
  64
  ```

* Example: Folding a list into a new list with `(:)`:

  ```haskell 
  foldr (:) [] [1..3]
  1 : (2 : (3 : []))
  1 : (2 : [3])
  1 : [2, 3]
  [1, 2, 3]            -- works
  
  foldl (:) [] [1..3]
  (([] : 1) : 2) : 3   -- error
  
  foldl (flip (:)) [1..3] 
  (([] : 1) : 2) : 3
  ([1] : 2) : 3
  [2,1] : 3
  [3,2,1]              -- works 
  ```

* The type of `(:)` requires that a value be the 1st argument and 
  a list be the 2nd argument:
  
  ```haskell 
  (:) :: a -> [a] -> [a]
  
  foldr (:) [] (1 : 2 : 3 : [])
  1 : (2 : (3 : []))
                -- satisfies (:) signature 
                
  foldl (:) [] (1 : 2 : 3 : [])
  (([] : 1) : 2) : 3
    -- doesn't work; order of arguments backward for (:)
  ```

* `flip` allows a function to take its arguments backwards.

  ```haskell 
  foldl (flip (:)) [] (1 : 2 : 3 : [])
  (([] `f` 1) `f` 2) `f` 3
  ([1] `f` 2) `f` 3
  [2,1] `f` 3
  [3,2,1]
  ```

* `const` takes two arguments and always returns the first one.

* When we fold `const` over a list, it will take as its first pair of 
  arguments the `acc` value and a value from the list.
  
  * Which value it takes first depends on which type of fold it is.
  
  ```haskell
  -- Right Associativity
  
  foldr (flip const) 0 [1..5]
  1 `const` (2 `const` (3 `const` ( 4 `const' (5 `const` 0))))
      (0)          0          0          0           0
  
  foldr const 0 [1..5]
  1 `const` (2 `const` (3 `const` (4 `const` (5 `const` 0))))
      (1)          2          3          4           5

  -- Left Associativity
  
  foldl (flip const) 0 [1..5]
  ((((0 `const` 1 ) `const` 2) `const` 3) `const` 4) `const 5`
           1           2          3          4         (5)
           
  foldl const 0 [1..5]
  ((((0 `const` 1) `const` 2) `const` 3) `const` 4) `const` 5
           0          0          0          0         (0)
  ```

* Depending on your folding function, a left fold can lead to a different 
  result than a right fold of the same.
