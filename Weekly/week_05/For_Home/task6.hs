import Data.Char
import Data.List

main :: IO()
main = do
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30

switchSum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchSum f g 0 = (\ x -> 0)
switchSum f g n = (\ x -> f x + (switchSum g f (n - 1)) (f x))
