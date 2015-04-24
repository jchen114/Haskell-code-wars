
spam :: Int -> String
spam i
 | i < 0 = ""
 | i == 0 = ""
 | otherwise = "hue" ++ spam (i-1)