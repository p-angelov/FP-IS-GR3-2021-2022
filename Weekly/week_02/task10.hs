main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

rev :: Int -> Int
rev n = helper n 0
 where
     helper :: Int -> Int -> Int
     helper 0 result = result
     helper n result = helper (div n 10) (result * 10 + mod n 10)

isPalindrome :: Int -> Bool
isPalindrome x = rev x == x

countPalindromes :: Int -> Int -> Int
countPalindromes a b
  | a < 0 = error "value lower than 0"
  | b < 0 = error "value lower than 0"
  | otherwise = helper (min a b) (max a b) 0
  where
      helper :: Int -> Int -> Int -> Int
      helper a b count
       | a == b = count
       | a < b && isPalindrome (a + 1) = helper (a + 1) b (count + 1) 
       | otherwise = helper (a + 1) b count
