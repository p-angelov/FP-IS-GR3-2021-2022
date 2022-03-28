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

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence n dig
  | n < 0 = error "n must be positive"
  | otherwise = helper n 0
    where
      helper :: Int -> Int -> Int
      helper 0 res = res
      helper n res
        | mod n 10 == dig && res == 0 = div n 10
        | mod n 10 == dig = rev res + 10 ^ (digCount res) * (div n 10)
        | otherwise = helper (div n 10) (res * 10 + mod n 10)

biggestDig :: Int -> Int
biggestDig n = helper n 0
  where 
      helper :: Int -> Int -> Int
      helper 0 currMax = currMax
      helper num currMax
        | (mod num 10) > currMax = helper (div num 10) (mod num 10)
        | otherwise = helper (div num 10) currMax

digCount :: Int -> Int
digCount n
  | div n 10 == 0 = 1
  | otherwise =  1 + digCount (div n 10)

rev :: Int -> Int
rev n = helper n 0
  where
      helper :: Int -> Int -> Int
      helper 0 result = result
      helper n result = helper (div n 10) (result * 10 + mod n 10)

sortN :: Int -> Int
sortN n
  | n < 10 = n
  | otherwise = helper n (digCount n) 0
  where
      helper :: Int -> Int -> Int -> Int
      helper curr power res
        | curr == 0 = div res 10
        | otherwise = helper (removeFirstOccurrence curr (biggestDig curr)) (power - 1) (res + (biggestDig curr) * 10^power)