main :: IO()
main = do
    --print $ countDigitsIter (-13) -- error "n was negative"
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3

    --print $ countDigitsRec (-13) -- error "n was negative"
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsIter :: Int -> Int
countDigitsIter n
  | n < 0 = error "n was negative"
  | otherwise = helper n 0
  where
      helper :: Int -> Int -> Int
      helper n sum
        | n == 0 = sum
        | otherwise = helper (div n 10) (sum + 1)

countDigitsRec :: Int -> Int
countDigitsRec n
  | n < 0 = error "n was negative"
  | n >= 0 && n <= 9 = 1
  | otherwise = 1 + countDigitsRec (div n 10)