main :: IO()
main = do 
    print $ onlyArithmentic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]

isArithmetic :: [Int] -> Bool
isArithmetic xs = length xs < 2 || xs == take (length xs) [(xs!!0), (xs!!1) .. ]

onlyArithmentic :: [[Int]] -> [[Int]]
onlyArithmentic xss = [xs | xs <-xss, isArithmetic xs]