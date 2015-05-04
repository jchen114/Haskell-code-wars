module Codewars.Kata.MultiplesRedux where
import Data.List

solution :: Integer -> Integer
solution lng = 3 * (k (lng-1) 3 * ((k (lng-1) 3) + 1) `div` 2) + 5 * (k (lng-1) 5 * ((k (lng-1) 5)+1) `div` 2) - 15 * (k (lng-1) 15 * ((k (lng-1) 15)+1) `div` 2)
 where k n a0 = n `div` a0

-- Best Solution:
solution' :: Integer -> Integer
solution' n = s 3 + s 5 - s 15
  where s x = x * (pred n `div` x) * (1 + pred n `quot` x) `quot` 2  
