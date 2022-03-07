main :: IO()
main = do
    print $ countOccurences 121 1 == 2

countOccurences :: Int -> Int -> Int
countOccurences n num
  | n < 0 = error "n is negative"
  | n < 10 && n == num = 1
  | mod n 10 == num = 1 + countOccurences (div n 10) num
  | mod n 10 /= num = countOccurences (div n 10) num
  | otherwise = countOccurences (div n 10) num