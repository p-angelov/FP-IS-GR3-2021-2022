main :: IO()
main = do
    print $ removeFirstOccurrence 15365 5 -- == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

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
      helper :: Int -> Int -> Int -- flagg wit bool
      helper 0 res = res
      helper n res
        | mod n 10 == dig = res
        | otherwise = helper (div n 10) (res * 10 + mod n 10)
