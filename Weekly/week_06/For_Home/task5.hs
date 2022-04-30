import Data.Char
import Data.List

main :: IO()
main = do
    print $ line (1, 1) (2, 3) == 2.2360679774998

type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)