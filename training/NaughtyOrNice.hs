type Warrior = (String, Bool)

getNiceNames :: [Warrior] -> [String]
getNiceNames  xs = map fst $ filter f xs
 where f = snd

getNaughtyNames :: [Warrior] -> [String]
getNaughtyNames xs = map fst $ filter f xs
 where f (x,y) = not $ snd (x,y)