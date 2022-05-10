import Data.Char
import Data.List

main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6

reverseOrdSuff :: Int -> Int
reverseOrdSuff n = helper (mod n 10) (div n 10)
 where
     helper r l 
       | mod l 10 <= mod r 10 = r
       | otherwise = helper (r * 10 + mod l 10) (div l 10) 