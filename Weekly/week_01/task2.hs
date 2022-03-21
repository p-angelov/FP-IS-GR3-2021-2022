main :: IO()
main = do
    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000

    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000

sumCubesPow :: Int -> Int -> Int
sumCubesPow a b = a^3 + b^3

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow a b = a * a * a + b * b * b