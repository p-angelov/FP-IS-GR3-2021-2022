import Data.Char

main :: IO()
main = do
    print $ rev 123 == 321

    print $ isPrime 5 == True
    print $ isPrime 6 == False
    print $ isPrime 11 == True
    print $ isPrime 13 == True

    print $ sumDig 142500 == 12

    print $ sumDivs 161 == 192

rev :: Int -> Int
rev n = read $ reverse $ show n
--rev = read . reverse . show

isPrime :: Int -> Bool
isPrime n = n > 1 && [1, n] == [d <- [1 .. n], mod n d == 0]

sumDig :: Int -> Int
sumDig n = sum $ map digitToInt show n
-- sumDig = sum . map digitToInt . show

sumDivs :: Int -> Int
sumDivs n = [d | d <- [1 .. n], mod n d == 0]