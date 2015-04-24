import Data.Char

squareDigit :: Int -> Int
squareDigit num
 | num < 0 = -1 * squareDigits (-1 * num)
 | otherwise = squareDigits num

squareDigits :: Int -> Int
squareDigits num = intsToInt $ map digitToInt $ concat' $ map numToChar $ map (^2) $ map digitToInt $ numToChar num

numToChar :: Int -> String
numToChar num
 | num < 10 = [intToDigit num]
 | otherwise = numToChar (num `div` 10) ++ [intToDigit $ num `mod` 10]

intsToInt :: [Int] -> Int
intsToInt ns = foldr (\a b -> a + 10*b) 0 $ reverse ns

concat' :: [String] -> String
concat' ns = foldl (++) "" ns