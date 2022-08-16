import Data.Char
import Data.List

main :: IO()
main = do
    print $ (kthNumber [-2, 3, 5, -4, -13, -15, 20, -21] (>= 5)) 2 == 20
    print $ (kthNumber [-2, 3, 5, -4, -13, -15, 20, -21] (> 5)) 12 == error "No such number"

kthNumber :: [Int] -> (Int -> Bool) -> (Int -> Int)
kthNumber xs p = (\ k -> if k > length [x | x <- xs, p x] then error "No such number!" else last [x | x <- xs, p x])