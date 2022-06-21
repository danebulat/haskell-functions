module FunctionFunctor where

-- Functor typeclass:
class MyFunctor f where
  myfmap :: (a -> b) -> f a -> f b

-- -------------------------------------------------------------------
-- In general, functions are parameterized by **two types**:
--
--     a -> b    or    (->) a b
--
-- To fit Functor, we can leave only the return type arbitrary,
-- written as:
--
--     (->) a
--
--     :kind (->)
--     (->) :: * -> * -> *
--
--     :kind ((->) Int)
--     (->) Int :: * -> *

-- -------------------------------------------------------------------
-- Where `FuncWithArgA` is `(->) a`, the type of `fmap` would be:
--
--     fmap :: (b -> c) -> FuncWithArgA b -> FuncWithArgA c
--
-- We map a function (b -> c) onto `FuncWithArgA b` to produce
-- `FuncWithArgA c`.
--
-- Going back to applying functor ((->) a) to fmap:
--
--     fmap :: (b -> c) -> ((->) a) b -> ((->) a) c
--
--     Alternatively
--
--     fmap :: (b -> c) -> (a -> b) -> (a -> c)
--     * The type of fmap for functions (function composition)

-- -------------------------------------------------------------------
-- One way to make this work:
--
--     instance Functor ((->) a) where
--       fmap g ff = \x -> g (ff x)
--
-- Note that `\x -> g (ff x)` is precisely the composition of
-- `g` onto `ff`, or `g . ff` in Haskell notation.
--
-- Using point-free notation to rewrite the instance:
--
--     instance Functor ((->) a) where
--       fmap g ff = (.)

-- -------------------------------------------------------------------
-- Examples

f1 :: String
f1 = fmap show (replicate 4) 5
-- "[5, 5, 5, 5]"

f2 :: String 
f2 = fmap show (replicate 4) (Just 6)
-- "[Just 6, Just 6, Just 6, Just 6]"

f3 :: [String]
f3 = (fmap . fmap) show (replicate 4) (Just 6)
-- ["Just 6", "Just 6", "Just 6", "Just 6"]

f4 :: [Maybe String]
f4 = (fmap . fmap . fmap) show (replicate 4) (Just 6)
-- [Just "6", Just "6", Just "6", Just "6"]
