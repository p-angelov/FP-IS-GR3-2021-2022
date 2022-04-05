import Data.Char
import Data.List
main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1

applyN :: (Eq a, Num a) => (a -> a) -> a -> (a -> a)
applyN f 1  = (\ x -> f x)
applyN f n  = (\ x -> (applyN f (n - 1)) (f x))