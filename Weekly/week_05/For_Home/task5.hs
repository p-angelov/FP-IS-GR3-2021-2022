import Data.Char
import Data.List

main :: IO()
main = do
    print $ (pairCompose [(\x -> x + 1), (\x -> x + 2), (\x -> x + 3)]) 1 == 8

pairCompose :: [(Int -> Int)] -> (Int -> Int)
pairCompose [] = (\ x -> x)
pairCompose [f] = (\ x -> (f.id) x)
pairCompose (f1:f2:fs) = (\ x -> (f1.f2) x + (pairCompose fs) x)