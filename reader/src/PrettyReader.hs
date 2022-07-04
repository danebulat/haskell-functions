{-# LANGUAGE NoImplicitPrelude #-}

module PrettyReader where

-- -------------------------------------------------------------------
-- Specialised signatures for function instances.
-- Side-by-side:
--
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
-- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
-- (=<<) :: (a -> r -> b) -> (r -> a) -> (r -> b)
--
-- Speaking generically in terms of algebras alone, you cannot get
-- a Monad instance from the Applicative. You can get an Applicative
-- from a Monad.
--
-- We use `flip` and apply `(<*>)` to make the Monad instance.
--
--    m >>= k = flip k <*> m
--
--    >  flip (a -> r -> b) <*> (r -> a)
--    >       (r -> a -> b) <*> (r -> a)
--
-- From here, we can compose the functions using the ((->) r)
-- Applicative implementation.
--
-- Ex: Composing functions with the function Monad:
--     let m = (+1) >>= (*)
--     m 2 = 6
-- -------------------------------------------------------------------

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

const :: a -> b -> a
const a b = a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \a -> f (g a)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

instance Functor ((->) r) where
  fmap = (.)
  --fmap f g = \x -> f (g x)

instance Applicative ((->) r) where
  pure = const
  f <*> a = \r -> f r (a r)

instance Monad ((->) r) where
  return = pure
  m >>= k = flip k <*> m

