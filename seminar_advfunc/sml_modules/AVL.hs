module AVL (AVLTree, find, insert, empty, delete) where
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
balanceRight node@(Node _ leftNode nodeValue rightNode)
     | rightBalanceFactor == 1  = rotateLeft $ updateDepth $ Node 0 leftNode nodeValue (rotateRight rightNode)
     | rightBalanceFactor == 0  = rotateLeft node
     | rightBalanceFactor == -1 = rotateLeft node
     where     Node _ subtreeRL valueR subtreeRR = rightNode
               rightBalanceFactor = (depth subtreeRL) - (depth subtreeRR)
               
balanceLeft :: AVLTree a -> AVLTree a
balanceLeft node@(Node _ leftNode nodeValue rightNode)
     | leftBalanceFactor == 1  = rotateRight node
     | leftBalanceFactor == 0  = rotateRight node
     | leftBalanceFactor == -1 = rotateRight $ updateDepth $ Node 0 (rotateLeft leftNode) nodeValue rightNode
     where     Node leftDeg subtreeLL valueL subtreeLR = leftNode
               leftBalanceFactor = (depth subtreeLL) - (depth subtreeLR)

rebalance :: AVLTree a -> AVLTree a
rebalance (Node _ nodeLeft nodeValue nodeRight)
     | balanceFactor == -2 = balanceRight node
     | balanceFactor `elem` [-1, 0, 1] = node
     | balanceFactor == 2 = balanceLeft node
     where     balanceFactor = (depth nodeLeft) - (depth nodeRight)
               node = updateDepth $ Node 0 nodeLeft nodeValue nodeRight

removeRightmostElement :: AVLTree a -> (a, AVLTree a)
removeRightmostElement (Node depth leftNode nodeVal Leaf) = (nodeVal, leftNode)
removeRightmostElement (Node depth leftNode nodeVal rightNode) = (removedValue, node')
     where     (removedValue, rightNode') = removeRightmostElement rightNode
               node' = updateDepth $ Node 0 leftNode nodeVal rightNode'

find :: Ord a => a -> AVLTree a -> Bool
find x Leaf = False
find x (Node deg leftNode nodeValue rightNode)
    | x == nodeValue = True
    | x <  nodeValue = find x leftNode
    | x >  nodeValue = find x rightNode

insert :: Ord a => a -> AVLTree a -> AVLTree a
insert x Leaf = Node 1 Leaf x Leaf
insert x node@(Node _ leftNode nodeValue rightNode)
    | x == nodeValue = node
    | x < nodeValue  = rebalance nodeL
    | x > nodeValue  = rebalance nodeR
    where     insertedIntoLeft = insert x leftNode
              insertedIntoRight = insert x rightNode
              nodeL = updateDepth $ Node 0 insertedIntoLeft nodeValue rightNode
              nodeR = updateDepth $ Node 0 leftNode nodeValue insertedIntoRight

delete :: Ord a => a -> AVLTree a -> AVLTree a
delete x Leaf = Leaf -- moglibysmy tez rzucac wyjatek
delete x node@(Node _ leftNode nodeValue rightNode)
    | x == nodeValue = if leftNode == Leaf then rightNode else node'
    | x < nodeValue = rebalance $ updateDepth $ Node 0 (delete x leftNode) nodeValue rightNode
    | x > nodeValue = rebalance $ updateDepth $ Node 0 leftNode nodeValue (delete x rightNode)
    where     (removedElement, leftNode') = removeRightmostElement leftNode
              node' = updateDepth $ Node 0 leftNode' removedElement rightNode

empty :: AVLTree a
empty = Leaf
