import Data.Char
import Data.List

main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69

specialSum :: Int -> Int -> Int
specialSum x y = sum [x | x <- [(min x y) .. (max x y)], mod (x - 1) 4 == 0 && elem 6 (map digitToInt $ show x)]