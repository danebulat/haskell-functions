module BinaryTree where 

import Data.List (sort)
import Data.Maybe (isNothing)

-- -------------------------------------------------------------------
-- In some cases, binary trees can be more efficient for structuring
-- and accessing data than a list, especially if you know how to order
-- your values in a way that lets you know whether to look “left” or
-- “right” to find what you want. On the other hand, a tree that only
-- branches to the right is indistinguishable from an ordinary list.
-- -------------------------------------------------------------------

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- unfold
-- -------------------------------------------------------------------

-- (a -> Maybe (a, b, a)
-- Takes value `a` and outputs `(a, b, a)` where `b` is the
-- node value.

-- case implementation
unfold :: (a -> Maybe (a, b, a))     -- node data generator function 
       -> a                          -- starting value for tree 
       -> BinaryTree b               -- output
unfold f val =                       -- where `val` is current value for tree
  case f val of
    Nothing -> Leaf
    Just (a, b, a') -> Node (unfold f a) b (unfold f a')

-- guards implementation 
unfold' :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold' f x
  | isNothing (f x) = Leaf
  | otherwise =
      let Just (a, b, a') = f x
       in Node (unfold' f a) b (unfold' f a')

-- Tree builders
-- -------------------------------------------------------------------

-- Using lambda 
treeBuild :: Integer -> BinaryTree Integer 
treeBuild n = unfold (\b -> if b == n then Nothing else Just (b+1, b, b+1)) 0

-- Using where clause 
treeBuildStr :: Integer -> BinaryTree String
treeBuildStr endVal =
  unfold genNode 0
  where 
    genNode :: Integer -> Maybe (Integer, String, Integer)
    genNode val =
      if val == endVal
        then Nothing
        else Just (val+1, show val, val+1)


