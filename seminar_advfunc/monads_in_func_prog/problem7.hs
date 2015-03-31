import Control.Monad.Cont

mcallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
mcallCC f = cont $ \k -> runCont (f (\a -> cont $ \_ -> k a)) k

-- callCCexp :: (Show a) => ((a -> Cont String b) -> Cont String a) -> Cont String a
-- callCCexp f = cont $ \shw -> runCont (f (\a -> cont $ \_ -> shw a)) show
callCCexp f = cont $ \k -> runCont (f (\a -> cont $ \_ -> k a)) k

cont $ \l -> (\k -> runCont (f (\a -> cont $ \_ -> k a)) k) (\a -> runCont (h a) l)
cont $ \l -> (\k -> runCont (f (\a -> cont $ \_ -> k a)) k) (\a -> runCont (h a) l)
cont $ \l -> runCont (f (\a -> cont $ \_ -> (\a -> runCont (h a) l) a)) (\a -> runCont (h a) l)
cont $ \l -> runCont (f (\a -> cont $ \_ -> (\a -> runCont (h a) l) a)) (\res -> runCont (return (100 + res)) l)

callCCexp :: ((a -> Cont r b) -> Cont r a) -> Cont r a
-- callCCexp f = cont $ \k -> runCont (f (\a -> cont $ \_ -> k a)) k
-- callCCexp f = cont $ \l -> runCont (cont foo) l
--      where foo k = runCont ((f (\res -> cont $ \_ -> k res)) >>= (\res -> cont $ \_ -> k res)) k
callCCexp f = cont $ \l -> runCont (cont foo) l
     where foo k = runCont ((f (\res -> cont $ \_ -> k res)) >>= (\res -> cont $ \_ -> k res)) k



-- callCCexp2 f = cont $ \k -> runCont (f (\a -> cont $ \_ -> k a)) (\res -> cont $ \_ -> k res)
-- callCCexp f = f (\a -> cont $ \_ -> k a)

-- k' x = \k -> k (100 - x)
-- k'' x = k' x show

k :: Int -> String
k = (\x -> ((\k' -> k' (100 - x)) show))

g :: Int -> Cont String a
g = \a -> cont $ \_ -> k a

f1 :: Int -> (Int -> Cont r ()) -> Cont r Int
f1 x = \ok -> do
     when (x > 10) $ ok (x * 2)
     when (x > 0) $ ok x
     return (-1)

divExc1 :: Int -> Cont r Int
divExc1 x = do
     res <- callCC $ \ok -> do
          when (x > 0) $ ok x
          return (-1)
     return (100 + res)

-- divExc2 :: Int -> Cont r Int
-- divExc2 x = (callCC (f1 x)) >>= (\res -> return (100 + res))
-- divExc2 x = cont $ \k -> s (\a -> runCont (f a) k)
     -- where s = runCont $ callCC (f1 x)
           -- f = (\res -> return (100 + res))

-- divExc2 :: Int -> Cont String Int
-- divExc2 x = do
--      res <- callCCexp $ \ok -> do
--           when (x > 10) $ ok (x * 2)
--           when (x > 0) $ ok x
--           return (-1)
--      return (100 - res)

divExc2 :: Int -> Cont r Int
divExc2 x = do
     res <- callCCexp $ \ok -> do
          when (x > 0) $ ok x
          return (-1)
     return (100 + res)

-- run1 x y = runCont (divExc1 x y (const $ cont ($ -1))) id
-- run2 x y = runCont (divExc2 x y (const $ cont ($ -1))) id

run1 x = runCont (divExc1 x) show
run2 x = runCont (divExc2 x) show
