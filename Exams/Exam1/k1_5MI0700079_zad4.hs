import Data.Char
import Data.List

main :: IO()
main = do
    print $ findSmallest [2, 5, 3, 7, 8, 1, 9] == [-1, 2, 2, 3, 7, -1, 1]
    print $ findSmallest [5, 7, 4, 9, 8, 10] == [-1, 5, -1, 4, 4, 8]
    print $ findSmallest [1, 5, 2, 2, 2, 5, 5, 4] == [-1, 1, 1, 1, 1, 2, 2, 2]

findSmallest :: (Num a, Ord a) => [a] -> [a]
findSmallest xs = map (\ )