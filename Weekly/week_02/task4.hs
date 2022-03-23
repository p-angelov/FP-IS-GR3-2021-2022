main :: IO()
main = do
    print $ countOccurences 121 1 == 2
    print $ countOccurences 222 1 == 0

countOccurences :: Int -> Int -> Int
countOccurences 0 num = 0
countOccurences n num
  | n < 0 = error "n is negative"
  | mod n 10 == num = 1 + countOccurences (div n 10) num
  | otherwise = countOccurences (div n 10) num