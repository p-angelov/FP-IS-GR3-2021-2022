main :: IO()
main = do
    print $ countOccurences 121 1 == 2

countOccurences :: Int -> Int -> Int
countOccurences n num
  | n < 0 = error "n is negative"
  | n >= 0 && n <= 9 && n == num = 1
  | otherwise = helper n num 0
  where
      helper :: Int -> Int -> Int -> Int
      helper n num count
        | n == num = count + 1
        | otherwise = helper (div n 10) num count