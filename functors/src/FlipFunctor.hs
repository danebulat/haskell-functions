{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where 

-- ---------------------------------------------------------------------------------------------------------
-- FlexibleInstances:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html
-- https://connectionrequired.com/blog/2009/07/my-first-introduction-to-haskell-extensions-flexibleinstances
-- ---------------------------------------------------------------------------------------------------------

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

-- Wraps functor and flips order of specifying type
-- constructor arguments 
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

-- Instance for (Flip Tuple a b)
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple b a)) = Flip (Tuple (f b) a)

-- Instance for (Flip Either a b)
instance Functor (Flip Either a) where
  fmap _ (Flip (Right a)) = Flip (Right a)
  fmap f (Flip (Left b)) = Flip (Left (f b))

-- Example use 
testFlip = let x = Flip (Tuple 10 "hello")
               y = Flip (Left  10)
            in ( fmap (*2) x
               , fmap (*2) y )

