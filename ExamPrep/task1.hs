import Data.Char
import Data.List

main :: IO()
main = do
    print $ applyEveryKth (* 2) 3 [1..6] == [1,2,6,4,5,12]
    print $ applyEveryKth (+ 2) 4 [1..7] == [1,2,3,6,5,6,7]

applyEveryKth :: (a -> a) -> Int -> [a] -> [a]
-- applyEveryKth f k xs =  [if mod i k == 0 then f x else x | (x, i) <- zip xs [1 .. ]]
applyEveryKth f k xs = map (\(x, i) -> if mod i k == 0 then f x else x) (zip xs [1 ..])