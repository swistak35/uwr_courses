{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad

-- dokomentowac czemu wywalilem monade
-- dokomentowac czemu dodalem Ord
-- class Set m a where
--   find :: m a -> a -> Bool
--   insert :: m a -> a -> m a
--   delete :: m a -> a -> m a
--   empty :: m a

-- instance (Show a, Eq a) => Set [] a where
--   find [] _ = False
--   find (x:xs) y = (x == y) || find xs y
  
--   insert [] y = [y]
--   insert (x:xs) y = if x == y 
--       then (x:xs)
--       else (x: insert xs y)
  
--   delete [] _ = []
--   delete (x:xs) y = if x == y 
--       then delete xs y 
--       else (x: delete xs y)

--   empty = []

class Ord a => MySet m a where
  find :: m a -> a -> Bool
  insert :: a -> m a -> m a
  -- delete :: m a -> a -> m a
  empty :: m a

data AVLTree a = Leaf
               | Node Int (AVLTree a) a (AVLTree a)
               deriving (Show, Eq)

depth :: AVLTree a -> Int
depth Leaf = 0
depth (Node d _ _ _) = d

parentDepth :: AVLTree a -> AVLTree a -> Int
parentDepth l r = (max (depth l) (depth r)) + 1

updateDepth :: AVLTree a -> AVLTree a
updateDepth (Node d l x r) = Node d' l x r
     where     d' = parentDepth l r

rotateRight node@(Node _ leftNode nodeValue rightNode) = updateDepth $ Node 0 subtreeLL valueL rightNode'
     where     Node _ subtreeLL valueL subtreeLR = leftNode
               rightNode' = updateDepth $ Node 0 subtreeLR nodeValue rightNode

rotateLeft node@(Node _ leftNode nodeValue rightNode) = updateDepth $ Node 0 leftNode' valueR subtreeRR
     where     Node _ subtreeRL valueR subtreeRR = rightNode
               leftNode' = updateDepth $ Node 0 leftNode nodeValue subtreeRL

balanceRight :: AVLTree a -> AVLTree a
balanceRight node@(Node deg leftNode nodeValue rightNode)
     | rightBalanceFactor == 1 = rotateLeft (updateDepth (Node deg leftNode nodeValue (rotateRight rightNode)))
     | rightBalanceFactor == 0 = rotateLeft node
     | rightBalanceFactor == -1 = rotateLeft node
     where     Node rightDeg subtreeRL valueR subtreeRR = rightNode
               rightBalanceFactor = (depth subtreeRL) - (depth subtreeRR)
               
balanceLeft :: AVLTree a -> AVLTree a
balanceLeft node@(Node deg leftNode nodeValue rightNode)
     | leftBalanceFactor == 1 = rotateRight node
     | leftBalanceFactor == 0 = rotateRight node
     | leftBalanceFactor == -1 = rotateRight (updateDepth (Node deg (rotateLeft leftNode) nodeValue rightNode))
     where     Node leftDeg subtreeLL valueL subtreeLR = leftNode
               leftBalanceFactor = (depth subtreeLL) - (depth subtreeLR)

rebalance :: AVLTree a -> a -> AVLTree a -> AVLTree a
rebalance nodeLeft nodeValue nodeRight
     | newBalanceFactor == (-2) = balanceRight newNode
     | newBalanceFactor == (-1) = newNode
     | newBalanceFactor == 0 = newNode
     | newBalanceFactor == 1 = newNode
     | newBalanceFactor == 2 = balanceLeft newNode
     where     newBalanceFactor = (depth nodeLeft) - (depth nodeRight)
               newDepth = (max (depth nodeLeft) (depth nodeRight)) + 1
               newNode = Node newDepth nodeLeft nodeValue nodeRight


instance (Ord a) => MySet AVLTree a where
     find Leaf x = False
     find (Node deg leftNode nodeValue rightNode) x
          | x == nodeValue = True
          | x <  nodeValue = find leftNode x
          | x >  nodeValue = find rightNode x

     insert x Leaf = Node 1 Leaf x Leaf
     insert x node@(Node deg leftNode nodeValue rightNode)
          | x == nodeValue = node
          | x < nodeValue = rebalance insertedIntoLeft nodeValue rightNode
          | x > nodeValue = rebalance leftNode nodeValue insertedIntoRight
          where     insertedIntoLeft = insert x leftNode
                    insertedIntoRight = insert x rightNode

     empty = Leaf

     -- delete Leaf x = Leaf -- moglibysmy tez rzucac wyjatek
     -- delete node@(Node deg leftNode nodeValue rightNode) x
     --      | x == nodeValue = node
     --      | x < nodeValue = delete leftNode x
     --      | x > nodeValue = delete rightNode x

emptyInt :: AVLTree Int
emptyInt = empty 

tree1 = insert 6 $ insert 5 $ insert 4 $ insert 3 $ insert 2 $ insert 1 $ emptyInt
tree2 = insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 5 $ insert 6 $ emptyInt
tree3 = insert 6 $ insert 5 $ insert 4 $ insert 1 $ insert 2 $ insert 3 $ emptyInt
tree4 = insert 1 $ insert 2 $ insert 3 $ insert 6 $ insert 5 $ insert 4 $ emptyInt

test1 = Node 3 (Node 2 (Node 1 Leaf 1 Leaf) 2 (Node 1 Leaf 3 Leaf)) 4 (Node 2 Leaf 5 (Node 1 Leaf 6 Leaf))
test2 = Node 3 (Node 2 (Node 1 Leaf 1 Leaf) 2 Leaf) 3 (Node 2 (Node 1 Leaf 4 Leaf) 5 (Node 1 Leaf 6 Leaf)) 
test3 = Node 3 (Node 2 (Node 1 Leaf 1 Leaf) 2 (Node 1 Leaf 3 Leaf)) 4 (Node 2 Leaf 5 (Node 1 Leaf 6 Leaf))
test4 = Node 3 (Node 2 (Node 1 Leaf 1 Leaf) 2 Leaf) 3 (Node 2 (Node 1 Leaf 4 Leaf) 5 (Node 1 Leaf 6 Leaf))

tests :: Bool
tests = all id [tree1 == test1, tree2 == test2, tree3 == test3, tree4 == test4]
