import Data.Char
import Data.List

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]

type Cylinder = (Double, Double)

rounder :: Double -> Double
rounder n = fromIntegral (round (n * 100)) / 100

cylVolume :: Double -> Double -> Double
cylVolume r h = pi * r * r * h

getVolumes :: [Cylinder] -> [Double]
getVolumes cylinders = map (\ x -> rounder x) [cylVolume r h | (r, h) <- cylinders]