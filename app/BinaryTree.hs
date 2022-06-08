module BinaryTree where 

import Data.List (sort)

-- -------------------------------------------------------------------
-- In some cases, binary trees can be more efficient for structuring
-- and accessing data than a list, especially if you know how to order
-- your values in a way that lets you know whether to look “left” or
-- “right” to find what you want. On the other hand, a tree that only
-- branches to the right is indistinguishable from an ordinary list.
-- -------------------------------------------------------------------

-- Definition
-- ----------

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- unfold
-- -------------------------------------------------------------------

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  Just (x, y, z) -> Node (unfold f x) y (unfold f z)

-- tree builder
-- -------------------------------------------------------------------
-- A tree builder using the unfold function above.

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfold (\b -> if b == x then Nothing else Just (b+1, b, b+1)) 0

-- Inserting into trees
-- --------------------
-- Left lesser, right greater is a common convention for arranging
-- binary trees.

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

-- Write map for BinaryTree 
-- ------------------------

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) =
  Node (mapTree f left) (f x) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer 
mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- Convert binary trees into lists
-- -------------------------------

preorder :: (Ord a) => BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) =
   [x] ++ preorder left ++ preorder right

inorder :: (Ord a) => BinaryTree a -> [a]
inorder = sort . preorder

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bdd news bears."

-- Write foldr for BinaryTree 
-- --------------------------

foldTree :: (Num a, Num b) => (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = foldTree f z' left -- 3. fold left nodes 
   where
   z'  = f a z''             -- 2. fold current val with right fold val
   z'' = foldTree f z right  -- 1. fold right nodes 

-- Function to test foldTree function 
testFold :: Bool
testFold = let result = foldTree (+) 0 (testTree :: BinaryTree Integer)
            in result == 6
