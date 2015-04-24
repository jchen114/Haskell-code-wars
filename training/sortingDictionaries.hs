import Data.List (sortBy)
import Data.Ord  (comparing)

sortDict :: Ord v => [(k,v)] -> [(k,v)]
sortDict dict = sortBy f dict
 where f (w,x) (y,z)
             | x < z = GT
             | x == z = EQ
             | x > z = LT
			 
-- Best Solution:
sortDict ':: Ord v => [(k,v)] -> [(k,v)]
sortDict' = sortBy (flip $ comparing snd)