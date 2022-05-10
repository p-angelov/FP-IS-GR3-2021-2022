import Data.Char
import Data.List

main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: [(Int -> Int)] -> (Int -> Int)
getOddCompositionValue fs = (\x -> foldr (\f acc -> f acc) x $ map fst $ filter (\(f, i) -> odd i) $ zip fs [0 .. ])