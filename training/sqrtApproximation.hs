import Prelude hiding (sqrt)

sqrtInt :: Integral n => n -> Either (n,n) n
sqrtInt n
 | n == 0 = Right 0
 | n == 1 = Right 1
 | n - (g n) ^ 2 > 0 = Left (g n, (g n) +1)
 | otherwise = Right (g n)
  where g n = last [x | x <- [1,2..(n-1)], x^2 <= n] 

-- Best Solution:
heron :: (Ord a, Fractional a) => a -> a
heron a = until pred next a
  where pred x = abs (a - x ^ 2) <= 1e-10
        next x = (x + a/x) / 2

sqrtInt' :: Integral n => n -> Either (n,n) n
sqrtInt' n = if k * k == n then Right k else Left (k, k + 1)
  where k = floor . heron . fromIntegral $ n