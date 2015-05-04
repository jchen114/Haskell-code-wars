import Data.Set (Set)
import qualified Data.Set as S
import Data.List (sort, group)
import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram str = foldr f True $ (group . sort)  $ map  toLower str
 where f a b = length a == 1 && b

isIsogram' :: String -> Bool
isIsogram' str = not $ foldr (\x r s -> S.member x s || r (S.insert x s))  (\s -> False) str S.empty