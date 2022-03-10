main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result = helper (div n 10) (result * 10 + mod n 10)

removeD :: Int -> Int -> Int
removeD dig n
  | n < 0 = error "n must be positive"
  | n < 10 && n == dig = 0
  | n < 10 && n /= dig = dig
  | otherwise = helper dig n 0 0
  where
      helper :: Int -> Int -> Int -> Int -> Int
      helper dig n i res
        | n < 10 = rev res + n * 10^i
        | mod n 10 /= dig = helper dig (div n 10) (i + 1) (res + ((mod n 10) * 10^i))
        | otherwise = helper dig (div n 10) i res