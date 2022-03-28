import Data.Char
import Data.List

main :: IO()
main = do
    print $ maxRotation 56789 == 68957
    print $ maxRotation 12490 == 29140

digitCount :: Int -> Int -- функция за пресмятане броя цифри в дадено число
digitCount n = helper n 0
  where
      helper :: Int -> Int -> Int
      helper n count
        | n <= 0 = count
        | otherwise = helper (div n 10) (count + 1)

rotate :: [a] -> [a] -- помощна функция на digRotator
rotate [] = []
rotate (a:as) = as ++ [a]

digRotator :: Int -> Int -> Int -- функция за завъртане
digRotator n keeper
  | keeper == 0 = (read . rotate . show) n
  | otherwise = read $ rotate $ drop keeper $ show n

maxRotation :: Int -> Int -- главна функция
maxRotation n = maximum $ helper n 0
  where
      helper :: Int -> Int -> [Int]
      helper curr keeper
        | keeper >= digitCount n - 1 = []
        | otherwise = res : helper res (keeper + 1)
          where
              res = read $ take keeper (show curr) ++ (rotate $ drop keeper (show curr))