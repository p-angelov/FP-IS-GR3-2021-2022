import Data.Char
import Data.List

main :: IO()
main = do
    print $ maxRotation 56789 == 68957
    print $ maxRotation 12490 == 29140

    print $ sumCountsIter 1 1 == 1
    print $ sumCountsIter 5123 1 == 19
    print $ sumCountsIter 1234 8 == 10
    print $ sumCountsIter 5555 5 == 10
    print $ sumCountsIter 65432 6 == 11
    print $ sumCountsIter 70000 1 == 11
    print $ sumCountsIter 123321 1 == 29

countOccurences :: Int -> Int -> Int -- помощна функция за намиране броя на срещания на d в x
countOccurences 0 num = 0
countOccurences n num
  | n < 0 = error "n is negative"
  | mod n 10 == num = 1 + countOccurences (div n 10) num
  | otherwise = countOccurences (div n 10) num

sumDigits :: Int -> Int -- помощна функция за намиране сбора на цифрите в дадено число
sumDigits n
  | n < 0 = error "n was negative"
  | n >= 0 && n <= 9 = n
  | otherwise = helper n 0
  where
      helper :: Int -> Int -> Int
      helper n sum
        | n == 0 = sum
        | otherwise = helper (div n 10) (sum + (mod n 10))

sumCountsIter :: Int -> Int -> Int
sumCountsIter x d
  | x < 1 = error "x isn't in range"
  | d < 0 || d > 9 = error "d isn't in range"
  | otherwise = sumDigits $ helper 1 0
  where
      helper :: Int -> Int -> Int -- в този helper итерираме през числата от 1 до x и на всяко
      helper currNum res          -- добавяме срещанията на d към res и когато стигнем чилсо след  
        | currNum > x = res       -- x връщаме резултата в основната функция и ползваме sumDigits за да намерим крайния резултат
        | otherwise = helper (currNum + 1) (countOccurences currNum d) + res

digitCount :: Int -> Int -- функция за пресмятане броя цифри в дадено число
digitCount n = helper n 0
  where
      helper :: Int -> Int -> Int
      helper n count
        | n <= 0 = count
        | otherwise = helper (div n 10) (count + 1)

rotate :: [a] -> [a] -- помощна функция преместваща най-лявото число като първо вдясно
rotate [] = []
rotate (a:as) = as ++ [a]

maxRotation :: Int -> Int -- главна функция
maxRotation n = maximum $ helper n 0
  where
      helper :: Int -> Int -> [Int] -- помощна функция ползваща rotate и съответно запазваща цифрите вляво
      helper curr keeper
        | keeper >= digitCount n - 1 = []
        | otherwise = res : helper res (keeper + 1)
          where
              res = read $ take keeper (show curr) ++ (rotate $ drop keeper (show curr))