import Data.Char
import Data.List

main :: IO()
main = do
   print $ (pad ["S U", 
                  "FMI", 
                  "F P"]) '|' == ["|||||", 
                                  "|S U|", 
                                  "|FMI|", 
                                  "|F P|", 
                                  "|||||"]
   print $ (pad [[1, 2, 3, 4], 
                  [4, 5, 6, 7]]) 99 == [[99, 99, 99, 99, 99, 99], 
                                        [99, 1, 2, 3, 4, 99], 
                                        [99, 4, 5, 6, 7, 99], 
                                        [99, 99, 99, 99, 99, 99]]

pad :: [[a]] -> (a -> [[a]])
pad xs = map (\ xs -> )