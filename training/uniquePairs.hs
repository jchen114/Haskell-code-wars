-- Find unique pairs for n.

projectPartners :: Integer -> Integer
projectPartners n = factorial' n `div` (factorial' 2 * factorial' (n-2))

-- or 

projectPartners' :: Integer -> Integer
projectPartners' n = n * (n-1) `div` 2

factorial' :: Integer -> Integer
factorial' n
 | n == 0 = 1
 | n == 1 = 1
 | otherwise = n * factorial' (n-1)