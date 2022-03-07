main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0

removeD :: Int -> Int -> Int
removeD dig n
  | n < 0 = error "n must be positive"
  | n < 10 && n == dig = 0
  | n < 10 && n /= dig = dig
  | mod n 10 == dig = n - removeD dig (mod n 10)
  | mod n 10 /= dig = (removeD dig (div n 10)) * 10 + mod n 10
  | otherwise = removeD dig (div n 10)