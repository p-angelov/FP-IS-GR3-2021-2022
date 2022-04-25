import Data.Char
import Data.List

main :: IO()
main = do
    print $ maxChain [(3,4), (5,6), (7,8)] == 3
    print $ maxChain [(9,14), (3,5), (4,7)] == 2

canChain :: (Int, Int) -> (Int, Int) -> Bool
canChain (x1, y1) (x2, y2) = y1 < x2

maxChain :: [(Int, Int)] -> Int
maxChain xs = length $ helper 0 []
  where
      helper :: Int -> [Int] -> [Int]
      helper counter res
        | length res == length xs = res
        | canChain ((permutations xs)!!counter) ((permutations xs)!!(counter + 1)) = helper (counter - 1) (((permutations xs)!!counter)++res)
        | otherwise = helper (counter - 1) res
