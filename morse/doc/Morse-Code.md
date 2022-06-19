# Morse Code (p.846)

- Project to translate text to and from Morse code.

- When using `stack new <prog-name>` (instead of `stack init` for an existing 
  project)
  
  - `Setup.hs` generated; for Cabal build system; usually shouldn't need  
    to touch it.
  
  ```bash 
  -- Example
  stack new morse new-template -p "author-name:value -p "author-email:value"
    -p "category:value" -p "copyright: value" -p "github-username:value"
  ```

## Note 

- Remove or rename `package.yaml` from stack project if specifying dependencies 
  executable, library and test components manually in the project's `.cabal` file.
  
- Otherwise, stack will keep on overwriting the `.cabal` file when building.

## Code Concepts 

- `Data.Map` is a balanced binary tree where each node is a pairing of a __key__
  and a __value__. 
  
- The __key__ must be orderable, Ie. have a `Ord` instance.

- We want to make a list of pairs, where each pair includes both the English-
  language character and its Morse code representation.
  
### `sequence`

- Evaluates each monadic action in the structure from left to right, and collects 
  the result. 
  
  ```haskell 
  sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
  
  -- Example 1
  sequence $ Right [1, 2, 3, 4]
  [Right 1, Right 2, Right 3, Right 4]
  
  sequence $ [Right 1, Right 2, Right 3, Right 4]
  Right [1, 2, 3, 4]
  ```

## The Main Event 

- A `Main` module that will handle our Morse code conversions.

- Finding the executable location with stack: 

  ```bash 
  stack exec which morse 
  ```
  
- To install the executable to a common directory:

  ```bash 
  -- Will install to $HOME/.local/bin
  stack install 
  ```

## Time to test!

- Tests written in `test/` directory.

  - Directory is specified in `.cabal` or `package.yaml` file.
  
- Import the modules containing functions you wish to test into the 
  test suite's `Main` module.

- Write appropriate generators for tests.

- Write tests for particular properties (QuickCheck).

## Testing the Morse Code 

- The property we're testing is that we get the same string after
  we convert it to Morse and back again.
  
- Load up test suite in GHCi:

  ```haskell 
  -- Load test component, specified in .cabal file
  stack ghci morse:morse-test 
  
  -- Run tests 
  main 
  ```

