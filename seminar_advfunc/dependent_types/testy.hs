res :: Maybe Int
res = (Just 3) >>= (\x -> case x of
  Just e -> return (e + 1)
  Nothing -> return 0)
