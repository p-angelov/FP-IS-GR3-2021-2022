main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14

everyOther :: Int -> Int
everyOther n
  | n < 100 = div n 10
  | otherwise = helper n 0 0
  where
      helper :: Int -> Int -> Int -> Int
      helper n i res
        | n < 100 = res
        | i == 1 = helper (div n 10) (i - 1) (res + (mod n 10)*10)
        | otherwise = helper (div n 10) i res
       