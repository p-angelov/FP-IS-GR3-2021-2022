main :: IO()
main = do
    print $ removeFistOccurrence 15365 5 == 1536
    print $ removeFistOccurrence 15360 0 == 1536
    print $ removeFistOccurrence 15300 0 == 1530
    print $ removeFistOccurrence 15365 1 == 5365
    print $ removeFistOccurrence 35365 3 == 3565
    print $ removeFistOccurrence 1212 1 == 122
    print $ removeFistOccurrence 1212 2 == 121
    print $ removeFistOccurrence (removeFistOccurrence 1212 1) 1 == 22

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result = helper (div n 10) (result * 10 + mod n 10)

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence n dig
  | n < 0 = error "n must be positive"
  | otherwise = helper n 0
  where
      helper :: Int -> Int -> Int
      helper 0 res = res
        | mod n 10 == dig = res + (n - dig * 10)
        | otherwise helper (div n 10) (res * 10 + mod n 10)
