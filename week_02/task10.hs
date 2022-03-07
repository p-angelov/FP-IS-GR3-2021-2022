main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

{-
Проблемът е бездънна рекурсия. Направи helper да работи при предположението,
че първият му аргумент е по-малък или равен на втория.
След това се опитай от главната функция да подадеш по-малкото от двете числа за първи аргумент и по-голямото за втори.

2. Опитай се да имаш най-много две условия в гард.
-}
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
  | a > b = countPalindromes b a
  | otherwise = helper a b 0
  where
      helper :: Int -> Int -> Int -> Int
      helper a b count
       | a <= b && isPalindrome (a + 1) = count + 1 
       | otherwise = helper (a + 1) b count
