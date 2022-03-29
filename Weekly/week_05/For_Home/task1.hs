import Data.Char ()
import Data.List ()

main :: IO()
main = do

    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

isPali :: Int -> Bool
isPali 0 = True
isPali x
  | x < 10 = True
  | head (show x) /= head $ reverse $ show x = False
  | otherwise = isPali (read $ tail $ init (show x))

getPalindromes :: Int -> Int
getPalindromes n = helper n 0
  where
      helper :: Int -> Int -> Int
      helper currDiv res
        | currDiv == 0 = res
        | isPali currDiv && mod n currDiv == 0 = helper (currDiv + 1) (res + currDiv)
        | otherwise = helper (currDiv + 1) res