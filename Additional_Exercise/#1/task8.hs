import Data.Char
import Data.List

main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45

onlyUnique :: [Int] -> Int
onlyUnique = sum . concat . filter ((==1) . length) . group . sort

sumUnique :: [[Int]] -> Int
sumUnique = sum . map onlyUnique

