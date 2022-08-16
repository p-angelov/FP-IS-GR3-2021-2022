import Data.Char
import Data.List

main :: IO()
main = do
    print $ seriesSum 1 3 == 20.0
    print $ seriesSum 2 4 == 64.0

seriesSum :: Double -> Int -> Double
seriesSum x n = helper 1 0
 where
     helper :: Int -> Double -> Double
     helper i res
      | n < i = res
      | otherwise = helper (i + 1) ((1 + x^i + (fromIntegral i^2)) + res)