main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result = helper (div n 10) (result * 10 + mod n 10)

sortN :: Int -> Int
sortN n
  | n < 10 = n
  | otherwise = helper n 0 0
  where
      helper :: Int -> Int -> Int -> Int
      helper n res dig
        | n < 10 = rev res
        | mod n 10 == dig && dig < 10 = helper (div n 10) (res + ((mod n 10) * 10)) (dig + 1)
        | otherwise = div n 10