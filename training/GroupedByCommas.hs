module Codewars.Commas where

--groupByCommas :: Int -> String
--groupByCommas n = show n

sectionBy3 :: [a] -> [[a]]
sectionBy3 as
 | length as > 3 = [takeFromLast 3 as] ++ sectionBy3 as
 | otherwise = [as]
 
takeFromLast :: Int -> [a] -> [a]
takeFromLast n as 
 | length as > n = reverse $ take n $ reverse as
 | otherwise = as