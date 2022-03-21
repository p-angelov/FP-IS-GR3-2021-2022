main :: IO()
main = do
    {-
    *N* is divisible by *d*
    and *N* is less than or equal to *b*
    and *N* is greater than 0.
    d b = N
    -}
    print $ maxMultiple 2 7 == 6
    print $ maxMultiple 3 10 == 9
    print $ maxMultiple 7 17 == 14
    print $ maxMultiple 10 50 == 50
    print $ maxMultiple 37 200 == 185
    print $ maxMultiple 7 100 == 98  
    print $ maxMultiple 7 10 == 7
    print $ maxMultiple 4 4 == 4
    
{-
divides :: Int -> Int -> Bool
divides a b = mod a b == 0

isMaxMultiple :: Int -> Int -> Int -> Bool
isMaxMultiple n d b = n <= b && divides n d

maxMultiple :: Int -> Int -> Int
maxMultiple d b
  | b > d = error "invalid input"
  | otherwise = helper d b 0
  where
      helper :: Int -> Int -> Int -> Int
      helper d b myMax
        | isMaxMultiple myMax d b = myMax
        | otherwise = helper d b (myMax + 1)
-}

maxMultiple :: Int -> Int -> Int
maxMultiple d 0 = error "division by 0"
maxMultiple d b
 | mod b d == 0 = b
 | otherwise = maxMultiple d (b - 1)