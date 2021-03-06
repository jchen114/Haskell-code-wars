import Data.List

-- Given two arrays of strings a1 and a2 return a sorted array in lexicographical order and without duplicates of the strings of a1 which are substrings of strings of a2.

-- Sorry for the name of the function.
inArray :: [String] -> [String] -> [String]
inArray [] _ = []
inArray _ [] = []
inArray (a1:a1s) a2
 | findString a1 a2 = sort $ foldl chk [] $ [a1] ++ inArray a1s a2
 | otherwise = inArray a1s a2
 where chk as el
        | el `elem` as = as
        | otherwise = [el] ++ as

findString :: String -> [String] -> Bool
findString a [] = False
findString a (b:bs) = a `isInfixOf` b || findString a bs

-- Best solution
inArray' :: [String] -> [String] -> [String]
inArray' a1 a2 = nub . sort . filter (\s -> any (s `isInfixOf`) a2) $ a1
