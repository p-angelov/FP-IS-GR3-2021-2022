main :: IO()
main = do
    print $ primesProd 12 == 6
    print $ primesProd 1200 == 200560490130

isPrime :: Int -> Bool
isPrime n = n > 1 && [1, n] == [ d | d <- [1 .. n], mod n d == 0]

primesProd :: Int -> Int
primesProd x = helper x 1
  where
      helper :: Int -> Int -> Int
      helper curr res
        | curr < 0 = error "X cannot be negative"
        | curr == 0 = res
        | sqrt $ fromIntegral curr > x && isPrime curr  = helper (curr - 1) (res * curr)
        | otherwise = helper (curr - 1) res
