import Data.Char

main :: IO()
main = do
    print $ checkNumber 2728 -- == (4,15)
    print $ checkNumber 31415 -- == (12,2)
    print $ checkNumber 121 -- == (2,2)

checkNumber :: Int -> (Int, Int)
checkNumber n = (sum (evenDigits $ listOfDigits n), sum (oddDigits $ listOfDigits n))
    where
     listOfDigits n = map (\ x -> digitToInt x) (show n)

evenDigits :: [Int] -> [Int]
evenDigits [] = []
evenDigits [x] = []
evenDigits (_:x:xs) = [x] ++ evenDigits xs

oddDigits :: [Int] -> [Int]
oddDigits [] = []
oddDigits [x]= [x]
oddDigits (x:_:xs) = [x] ++ oddDigits xs