{-# LANGUAGE FlexibleInstances #-}

module Exercises where 

import GHC.Arr

-- -------------------------------------------------------------------
-- Determine if a valid Functor can be written for the
-- datatype provided:

-- 1.
-- data Bool = True | False
-- A: No

-- 2.
data BoolAndSomethingElse a =
  False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True'  a) = True'  (f a)

-- 3.
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap f (Truish a) = Truish (f a)
  fmap _ Falsish = Falsish

-- 4.
-- newtype Mu f = InF { outF :: f (Mu f) }
-- A: No

-- 5.
-- data D = D (Array Word Word) Int Int 
-- A: No 

-- -------------------------------------------------------------------
-- Rearrange the arguments to the type constructor of the
-- datatype so the Functor instance works. 

-- 1.
data Sum b a =
  First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2.
data Company a c b =
  DeepBlue a c | Something b deriving (Eq, Show)

instance Functor (Company a c) where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

-- apply function to type 'a'
instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b' 

-- -------------------------------------------------------------------
-- Write Functor instances for the following datatypes.

-- 1.
data Quant a b =
  Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)
  
-- 2.
data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

-- 3.
-- {-# LANGUAGE FlexibleInstances  #-}
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

-- now the type arguments have been flipped, we can
-- apply the function on K's a argument.
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

-- 4
data EvilGoateeConst b a =
  GoatyConst b

instance Functor (EvilGoateeConst b) where
  fmap _ (GoatyConst a) = GoatyConst a


-- 5.
-- Do you need something extra to make the instance work?
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fu) = LiftItOut (fmap f fu)

-- 6.
data Parappa f g a =
  DaWrappa (f a) (g a)

-- apply function to type `a` which the two functors contain
instance (Functor f, Functor g)
       => Functor (Parappa f g) where
  fmap f (DaWrappa f' g') = DaWrappa (fmap f f') (fmap f g')

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)
  
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething f' g) = IgnoringSomething f' (fmap f g) 

-- 8.
data Notorious g o a t =
  Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious g1 g2 g3) = Notorious g1 g2 (fmap f g3)

-- 9.
-- You'll need to use recursion
data List a =
  Nil | Cons a (List a)

instance Functor (List) where
  fmap f (Cons a l) = Cons (f a) (fmap f l)
  fmap _ Nil = Nil 

-- 10.
-- A tree of goats forms a Goat-Lord, fearsome poly-creature.
data GoatLord a =
    NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- 11.
-- You'll need an extra functor for this one, although your
-- solution might do it monomorphically without using fmap.
-- Keep in mind that you will probably not be able to
-- validate this one in the usual manner. Do you best to
-- make it work.

data TalkToMe a =
  Halt | Print String a | Read (String -> a)

-- https://eli.thegreenplace.net/2018/haskell-functions-as-functors-applicatives-and-monads/
-- instance Functor TalkToMe where
--   fmap _ Halt = Halt
--   fmap (Print s a) = Print s (f a)

