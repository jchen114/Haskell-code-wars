-- Given the starting index of the ball, and the sequence of swaps,
-- return the final position of the ball.
findTheBall :: Int -> [(Int, Int)] -> Int
findTheBall start [] = start
findTheBall start (swap:swaps)
 | swaps == [] = swapper start swap
 | otherwise = findTheBall (swapper start swap) swaps
 
swapper :: Int -> (Int,Int) -> Int
swapper a (x,y)
 | a == x = y
 | a == y = x
 | otherwise = a
 
 -- Best solution
findTheBall' :: Int -> [(Int, Int)] -> Int
findTheBall' = foldl switch

switch current (from, to)
  | current == from = to
  | current == to = from
  | otherwise = current