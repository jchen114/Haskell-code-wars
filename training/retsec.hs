reverseByCenter :: String -> String
reverseByCenter xs
 | length xs `mod` 2 == 0 = (snd . f) xs ++ (fst  . f) xs 
 | otherwise = (tail . snd . f) xs ++ [(head . snd .  f) xs] ++ (fst . f) xs
  where f xs = splitAt (length xs `div` 2) xs
  
reverseByCenter' :: String -> String
reverseByCenter' xs =  
  let (q,r) = length xs `quotRem` 2
      (a,b) = splitAt q xs
  in if r == 0 then b ++ a
               else tail b ++ [head b] ++ a