import Data.Char
import Data.List

main :: IO()
main = do

    print $ reduceStr "dabAcCaCBAcCcaDD" -- == "dabCBAcaDD" 
    -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    
reduceStr :: String -> String
reduceStr [] = []
reduceStr (x:xs) = helper (x:xs)
 where 
     helper :: String -> String
     helper res
      | [x] = res
      | otherwise = res : reduceStr (filter (\y -> (toLower x == y) || (toLower y == x)) xs)