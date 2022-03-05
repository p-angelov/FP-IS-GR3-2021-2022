main :: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
     helper :: Int -> Bool
     helper i
      | (fromIntegral i) >= (sqrt $ fromIntegral n) = True
      | mod n i == 0 = False
      | otherwise = helper (i + 1)

sumPrimeDivs :: Int -> Int
sumPrimeDivs n
 | n < 0 = error "n has to be non-negative"
 | otherwise = helper 1 0
  where
      helper :: Int -> Int -> Int
      helper currentDiv result
       |currentDiv > n = result
       | mod n currentDiv == 0 && isPrime currentDiv = helper (currentDiv + 1) (result + currentDiv)
       | otherwise = helper (currentDiv + 1) result

