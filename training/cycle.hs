module Cycle where
import Data.List

data Direction = L | R deriving (Show)

-- Implement a function which when given the arguments
-- Direction to which to cycle the current value
-- List of values
-- Current value
-- returns the value next to current value in the specified direction.
-- The function should pick the next value from the other side of the list in case there are no values in the given direction.
-- Examples:
-- cycleList R [1,2,3] 1  -- => Just 2
-- cycleList L [1,2,3] 1  -- => Just 3
-- cycleList R [1,2,3] 0  -- => Nothing
-- cycleList L ["foo", "bar", "xyz"] "bar"  -- => Just "foo"

-- Mine
cycleList :: (Eq a) => Direction -> [a] -> a -> Maybe a
cycleList d vals val = case d of
 L -> elemIndex val vals >>= \x ->
  if x == 0 then Just $ last vals
  else Just $ vals !! (x - 1)
 R -> elemIndex val vals >>= \x ->
  if x == (length vals - 1) then Just $ head vals
  else Just $ vals !! (x + 1)

-- Best Solution  
cycleList' :: (Eq a) => Direction -> [a] -> a -> Maybe a
cycleList' d l v = lookup v $ case d of
                               L -> zip l' l
                               R -> zip l l'
  where l' = tail $ cycle l
  
-- Variations of mine with guards:
cycleList'' :: Eq a => Direction -> [a] -> a -> Maybe a
cycleList'' d vals val = elemIndex val vals >>= \x -> Just $ case d of
 L | x == 0                 -> last vals
    | otherwise              -> vals !! (x - 1)
 R | x == (length vals - 1) -> head vals
    | otherwise              -> vals !! (x + 1)
	
cycleList''' :: Eq a => Direction -> [a] -> a -> Maybe a
cycleList''' d vals val = do 
                                   x <- elemIndex val vals
                                   Just $ case d of
                                    L | x == 0 -> last vals
                                       | otherwise -> vals !! (x - 1)
                                    R | x == (length vals - 1) -> head vals
                                       | otherwise -> vals !! (x + 1)