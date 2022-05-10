import Data.Char

main :: IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)

-- checkNumber :: Int -> (Int, Int)
checkNumber n = foldr (\(v, i) (e, o) -> if even i then (e + v, o) else (e, o + v)) (0, 0) $ zip (map digitToInt $ show n)  [0 ..] 