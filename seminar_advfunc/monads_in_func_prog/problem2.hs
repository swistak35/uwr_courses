cfold f z [] = z
cfold f z (x:xs) = f x z (\y -> cfold f y xs)

efoldr :: (a -> b -> b) -> b -> [a] -> b
efoldr f z [] = z
efoldr f z (x:xs) = f x (efoldr f z xs)

mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr f z xs = cfold aux z xs
     where aux x y g = f x (g z)

efoldl :: (b -> a -> b) -> b -> [a] -> b
efoldl f z [] = z
efoldl f z (x:xs) = efoldl f (f z x) xs

mfoldl :: (b -> a -> b) -> b -> [a] -> b
mfoldl f z xs = cfold aux z xs
     where aux x y g = g (f y x)
