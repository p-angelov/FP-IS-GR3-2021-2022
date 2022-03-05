main :: IO()
main = do
    -- print $ sumDigitsIter (-13) -- error "n was negative"
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6

sumDigitsIter :: Int -> Int
sumDigitsIter n
  | n < 0 = error "n was negative"
  | n >= 0 && n <= 9 = n
  | otherwise = helper n 0
  where
      helper :: Int -> Int -> Int
      helper n sum
        | n == 0 = sum
        | otherwise = helper (div n 10) (sum + (mod n 10))