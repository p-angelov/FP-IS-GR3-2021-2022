main :: IO()
main = do
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumDigitsRec :: Int -> Int
sumDigitsRec 0 = 0
sumDigitsRec n = mod n 10 + sumDigitsRec (div n 10)

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k
  | finish < start = sumDivisibleNumbers finish start k
  | mod (sumDigitsRec start) k == 0 = 1 + (sumDivisibleNumbers (start + 1) finish k)
  | mod (sumDigitsRec start) k /= 0 = sumDivisibleNumbers (start + 1) finsih k
  | otherwise = 
  