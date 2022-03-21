main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789

rev :: Integer -> Integer
rev n
 | n <= 9 = n           
 | rev = helper
 where
     helper :: Integer
     helper = rem n 10 + n % 10
 | otherwise = rev * 10 + remainder
 where
     remainder = n % 10
{-
не успях да навържа логиката на "цикъла" във функционален стил
while(n != 0) 
{
    remainder = n % 10;
    reversed_number = reversed_number * 10 + remainder;
    n /= 10;
  }
-}