main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

{-
findSum :: Int -> Int -> Int -> Int
findSum a b n
  | n <= 3 = error "n must be bigger than 3"
  | otherwise = helper a b n 0 3 0
  where
      helper :: Int -> Int -> Int -> Int -> Int -> Int -> Int
      helper a b n currentDegree numCounter sum
        | numCounter == 0 = sum
        | currentDegree == n = helper a b (n - 1) 0 (numCounter - 1) sum
        | n - 3 <= currentDegree && currentDegree <= n - 1 = helper a b n (currentDegree + 1) numCounter (sum + (a + 2^currentDegree * b))
        | otherwise = helper a b n (currentDegree + 1) numCounter sum
-}

findSum :: Int -> Int -> Int -> Int
findSum a b n
  | n <= 3 = error "n must be bigger than 3"
  | otherwise = helper 0 0 a
  where
      helper :: Int -> Int -> Int -> Int
      helper currentDegree sum currentSum
        | currentDegree >= n = sum
        | n - 3 <= currentDegree && currentDegree <= n - 1 = helper (currentDegree + 1) (sum + currentSum + 2^currentDegree * b) (currentSum + 2^currentDegree * b)
        | otherwise = helper (currentDegree + 1) sum (currentSum + 2^currentDegree * b)