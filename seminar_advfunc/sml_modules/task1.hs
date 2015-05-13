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

import qualified AVL

class Ord a => MySet m a where
     find :: a -> m a -> Bool
     insert :: a -> m a -> m a
     delete :: a -> m a -> m a
     empty :: m a

instance (Ord a) => MySet AVL.AVLTree a where
     find   = AVL.find
     insert = AVL.insert
     delete = AVL.delete
     empty  = AVL.empty

