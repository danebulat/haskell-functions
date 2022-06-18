# Hangman game 

## Stack project 

- Using stack's `new` command to create a project:

  ```bash
  stack new hangman simple
  ```

- Basic project setup:

  ```bash 
  git clone https://github.com/haskellbook/hello
  ```

## Downloading words 

- Words installed to `/usr/share/dict` and copied into project 
  as `dict.txt`:

  ```bash
  pacman -S words 
  cp /usr/share/dict/words ./data/dict.txt
  ```
  
- Update `.cabal` so project can find `dict.txt`, and add 
  dependencies:

```bash
extra-source-files: data/dict.txt
build-depends: random, split
```

## Useful functions 

```haskell
isJust  (Data.Maybe)
all
all isJust [Just 12, Nothing, Just 22]

:t all :: (a -> Bool) -> [a] -> Bool 
:t all :: (a -> Bool) -> Maybe a -> Bool 
:t all :: (a -> Bool) -> Either b a -> Bool 

:t all 
all :: Foldable t => (a -> Bool) -> t a -> Bool

forever (Control.Monad)
exitSuccess (System.Exit)

randomRIO (System.Random)
randomRIO (0, 5) -- random number between 0 and 5

-- both split at newline (\n) character:
lines 
words
```
