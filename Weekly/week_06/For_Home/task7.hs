main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45   

initUnique :: [Int] -> [Int]
initUnique [] = []
initUnique (x:xs)
  | elem x xs = initUnique (filter (\ y -> y /= x) xs)
  | otherwise = [x] ++ initUnique xs

specialSum :: [Int] -> Int
specialSum xs = sum $ initUnique xs

sumUnique :: [[Int]] -> Int
sumUnique xss = sum $ helper xss
  where 
      helper :: [[Int]] -> [Int]
      helper [] = []
      helper (xs:xss) = [specialSum xs] ++ helper xss