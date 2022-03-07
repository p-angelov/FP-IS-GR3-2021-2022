main :: IO()
main = do
    --A number is interesting if and only if 
    --it is evenly divided by the sum of its digits.
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + sumDigits (div n 10)

isInteresting :: Int -> Bool
isInteresting n = mod n (sumDigits n) == 0