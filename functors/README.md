# Functor Code Examples 

## src 

- [`Exercises.hs`](./src/Exercises.hs)

  Making Functor instances for various types.
  
- [`FlipFunctor.hs`](./src/FlipFunctor.hs)

  Changing the order of type parameters of a Functor to apply 
  fmap over an "inner" type parameter.

- [`FunctorFunctor.hs`](./src/FunctionFunctor.hs)
  
  Implementation of `((->) a)` Functor istance. How `fmap` works 
  when applied to functions (just function composition).

- [`NaturalTransformation.hs`](./src/NaturalTransformation.hs)

  Simple example to change the __structure__ of a functor, but
  keep the values it contains untouched.

- [`ReplaceExperiment.hs`](./src/ReplaceExperiment.hs)

  Demonstrates composed `fmap` calls to apply a function to a 
  nested functor - a functor inside other functor(s).

## test 

- [`tests.hs`](./test/tests/hs)

  Demonstrates generating random functions with QuickCheck,
  using the `Fun` type in `QuickCheck.Test.Function`. Used to 
  property test the functor laws with different functor instances.
