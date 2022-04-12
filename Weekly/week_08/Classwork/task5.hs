import Data.Char
import Data.List

main :: IO()
main = do
    print $ (specialSum (5-) [1..10]) (> 0) == 30
    print $ (specialSum (+1) [(-5)..5]) odd == 45

specialSum :: (Int -> Int) -> [Int] -> ((Int -> Bool) -> Int)
specialSum f xs = (\ p -> sum $ map (\ x -> x^2) $ filter p $ map f xs)
