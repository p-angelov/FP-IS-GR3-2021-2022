import Data.Char
import Data.List
main :: IO()
main = do
    print $ sumCountsIter 1 1 == 1
    print $ sumCountsIter 5123 1 == 19
    print $ sumCountsIter 1234 8 == 10
    print $ sumCountsIter 5555 5 == 10
    print $ sumCountsIter 65432 6 == 11
    print $ sumCountsIter 70000 1 == 11
    print $ sumCountsIter 123321 1 == 29
{-
countOccurences :: Int -> Int -> Int
countOccurences 0 num = 0
countOccurences n num
  | n < 0 = error "n is negative"
  | mod n 10 == num = 1 + countOccurences (div n 10) num
  | otherwise = countOccurences (div n 10) num
-}

sumCountsIter :: Int -> Int -> Int
sumCountsIter x d
  | x < 1 = error "x isn't in range"
  | 