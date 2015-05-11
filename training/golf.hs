import Data.List

skip:: Int -> [a] -> [a]
skip n = map snd . filter (\x -> mod (fst x) n == 0) . zip [1..]

skips:: [a] -> [[a]]
skips xs = zipWith ($) (map skip [1..]) $ replicate (length xs) xs