-- Exercise 1

data MappedList a = MappedList Bool [a]
     deriving (Show, Eq)
instance Functor MappedList where
     fmap f (MappedList _ xs) = MappedList True (map f xs)

-- No. Functor has to obey law
--      fmap id == id
-- This implementation disobey this law.
--      id (MappedList False []) == MappedList False []
--   but
--      fmap id (MappedList False []) == MappedList True []
--      
-- However, I've assumed that we compare the values by default Eq implementation.
-- One could write such implementation of instance Eq (MappedList a) that compare only lists inside,
-- and then the law would be always satisfied.



-- Exercise 2
-- The Either type.
-- https://hackage.haskell.org/package/base-4.2.0.0/docs/Data-Either.html

-- Exercise 3
-- No, it's not.
-- Applicative laws:
--      pure id <*> u == u -- identity
--      pure (.) <*> u <*> v <*> w == u <*> (v <*> w) -- composition
--      pure f <*> pure x = pure (f x) -- homomorphism
--      u <*> pure y = pure ($ y) <*> u -- interchange
-- First of all, first flaw is not obeyed.
--      pure id <*> (ZipList [1,2,3]) == (ZipList [1])
-- We can fix it by implementing pure function like this:
--      pure x = ZipList (repeat x)

newtype ZipList a = ZipList {
     getZipList :: [a]
} deriving Show -- from hackage.haskell.org
instance Functor ZipList where
     fmap f (ZipList xs) = ZipList (map f xs)
instance Applicative ZipList where
     (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
     pure x = ZipList (repeat x)

-- Exercise 4
transp' :: [[a]] -> [[a]]
transp' [] = []
transp' [xs] = map pure xs
transp' (xs:xss) = getZipList $ pure (:) <*> (ZipList xs) <*> (ZipList $ transp' xss)

-- Exercise 5
instance Monad ZipList where
     return = ZipList . repeat
     (ZipList []) >>= _ = ZipList []
     (ZipList (x:xs)) >>= f = ZipList (x' : xs')
          where
               x' = head $ getZipList $ f x
               xs' = getZipList ( (ZipList xs) >>= zlTail . f )
               zlTail (ZipList xs) = ZipList (tail xs)

-- Problem is, all arrays inside ZipList has to be the same length.
-- For example, following works:
--      (return 1 >>= (\x -> ZipList $ repeat x))
-- but that one don't:
--      (return 1 >>= (\x -> ZipList [x]))

-- Thus, it violates left identity monadic law, that
--      return a >>= f == f a
-- because:
--      (\x -> ZipList [x]) 1
-- works fine.


-- Exercise 6a

pure' :: (Monad m) => a -> m a
pure' = return

-- fapp == <*>
fapp :: (Monad m) => m (a -> b) -> m a -> m b
fapp mf mx = do f <- mf
                x <- mx
                return (f x)

fmap' :: (Monad m) => (a -> b) -> m a -> m b
fmap' f mx = mx >>= (return . f)

-- Exercise 6b
-- instance Monad (Either' e) where
--      return = Right'
--      Right' x >>= f = f x
--      Left' e >>= f = Left' e

data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (Either' a) where
     fmap f (Left' e) = Left' e
     fmap f (Right' x) = Right' (f x)
instance Applicative (Either' a) where
     pure = Right'
     (Left' f) <*> _ = Left' f
     (Right' f) <*> (Left' x) = Left' x
     (Right' f) <*> (Right' x) = Right' (f x)
     -- mf <*> mx = mf >>= (\f -> mx >>= (\x -> return (f x)))
