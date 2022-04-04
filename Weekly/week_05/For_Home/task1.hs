import Data.Char
import Data.List

main :: IO()
main = do

    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

isPali :: Int -> Bool
isPali n = n == revInt n
  where
      revInt :: Int -> Int
      revInt = read . reverse . show

getPalindromes :: Int -> Int
getPalindromes n = (minimum xs) + (maximum xs)
  where
      xs = [x | x <- [2 .. n], isPali x && mod n x == 0]