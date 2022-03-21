main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
     helper :: Int -> Bool
     helper i
      | (fromIntegral i) > (sqrt $ fromIntegral n) = True
      | mod n i == 0 = False
      | otherwise = helper (i + 1)

truncatablePrime :: Int -> Bool
truncatablePrime n
  | n < 10 && isPrime n = True
  | isPrime n && isPrime (div n 10) = truncatablePrime (div n 10)
  | otherwise = False