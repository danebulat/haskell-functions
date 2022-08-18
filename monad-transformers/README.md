# Monad Transformers

## Overview

Implementations of the essential monad transformers in Haskell. Each  monad 
transformer has instances of `Functor`, `Applicative`, `Monad`, `MonadTrans` 
and `MonadIO`. Source code in `src/MonadTransformers.hs`:

### MaybeT

```haskell 
newtype MaybeT m a = 
  MaybeT { runMaybeT :: m (Maybe a) }
```

### EitherT (ExceptT)
  
```haskell 
newtype EitherT e m a = 
  EitherT { runEitherT :: m (Either e a) }
```

### ReaderT

```haskell 
newtype ReaderT r m a = 
  ReaderT { runReaderT :: r -> m a }
```

### StateT

```haskell
newtype StateT s m a = 
  StateT { runStateT :: s -> m (a, s) }
```

### IdentityT

```haskell 
newtype IdentityT m a = 
  IdentityT { runIdentityT :: m a }
```
