import Data.Char
import Data.List

main :: IO()
main = do
    print $ dominates (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    print $ dominates (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True
                        -- [3, 6, 9] [3, 4, 5]

dominatesFold :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominatesFold f g = foldl (\ acc x -> f x >= g x) True

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = all (\ x -> f x >= g x) xs