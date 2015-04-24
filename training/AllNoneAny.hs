all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr g True xs
 where g x y = f x && y
 
none' :: (a -> Bool) -> [a] -> Bool
none' f xs = not $ foldr g False xs
 where g x y = (f x) || y
 
any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr g False xs
 where g x y = (f x) || y 