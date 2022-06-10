# Building Projects

## Working with a basic project 

- Using stack:
 
  ```haskell 
  stack new proj-name simple`
  ```

## Building the project 

- Using stack:

  ```haskell 
  stack build 
  ```

- If build cannot find GHC, run `stack setup` setup. Determines which version of
  GHC you need based on the LTS snapshot specified in project's `stack.yaml` file.

  ```haskell 
  stack setup 
  ```

- In `stack.yaml`:

  ```haskell
  resolver: lts-18.28   -- for ghc-8.10.7
  ```

LTS snapshots listed here: https://stackage.org

## Loading and running code from the REPL

```haskell
stack ghci 

> :l Main   -- load Main module
```

## Stack `exec`

- Stack knows the paths where any executables might be located. So you 
  don't need to type the full path with `stack exec`.

  ```haskell
  stack exec proj
  ```

## Executable stanzas in Cabal files

- Stack creates an executable because of the following stanza in the
  project's `.cabal` file:

  ```haskell
  executable proj                     -- proj is name of executabe 
    hs-source-dirs:   src             -- where to look for source code 
    main-is:          Main.hs         -- where to find Main module & main function 
    default-language: Haskell2010     -- version of Haskell standard to expect 
    build-depends:    base >= 4.7 && < 5  -- dependencies 
  ```

### About executables and libraries 

- An __executable__ stanza is appropriate for making a command-line 
  application to run and use.
  
- When writing code we want people to be able to reuse in other projects,
  we need a __library__ stanza in the `.cabal` file and choose which 
  modules we want to expose.

## Making our project a library 

- First, add a library stanza to `.cabal` file:

  ```haskell
  library 
    hs-source-dirs:   src 
    exposed-modules:  Hello 
    build-depends:    base >= 4.7 && < 5
    default-language: Haskell2010
  ```

- GHC will warn to add library modules in the `.cabal` file's executable 
  stanza under `other-modules`.

- Add library to `build-depends` in `.cabal` file's executable stanza. 

- May need to add directory to source code for library to `hs-source-dirs` 
  in `.cabal` file's executable stanza.

## Module exports 

- By default, when you don't specify any exports in a module, every top-level
  binding is exported and can be impoted by another module.
  
## More on importing modules 

- The `:browse` command allows us to see what functions are included in the 
  named module. 

  ```haskell 
  :browse Data.Bool 
  ```

- To turn off Prelude when loading GHCi:

  ```haskell 
  -- outside of any projects 
  stack ghci --ghci-options -XNoImplicitPrelude 
  ```

## Qualified imports 

- Where all items must be qualified with `Data.Bool`:

  ```haskell 
  import qualified Data.Bool
  ```

- To set a temporary custom prompt; you can set a permanent prompt in 
  GHCi's configuration file. 
  
  ```haskell 
  :set prompt "Lambda> "
  ```

## `do` syntax and IO

- It's considered bad style to use `do` in single-line expressions as 
  it's not necessary. 
  
- Use `>>=` in single-line expressions instead of `do`.

## Useful functions 

```haskell
bool
```
