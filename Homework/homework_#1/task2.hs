import Data.Char
import Data.List
main :: IO()
main = do
    print $ maxRotation 56789 == 68957
    print $ maxRotation 12490 == 29140

