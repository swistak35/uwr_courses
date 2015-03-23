map :: (a -> b) -> [a] -> [b]
map f [] = []
map f x:xs = f x:map f xs

map2 :: (a -> b) -> [a] -> [b]
