import Data.Char
import Data.List

main :: IO()
main = do
    print $ getClosestDistance [(4, 5, 6), (2, 5, 10), (5, 2, (-10)), ((-2), 1, 45), (12, 0, 2), (6, 5, 4)] == 2.83

type Point = (Int, Int, Int)

roundTwoDigButWithMagic :: Double -> Double
roundTwoDigButWithMagic = (/ 100) . fromIntegral . round . (* 100)

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt ( fromIntegral $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 )

getClosestDistance :: [Point] -> Double
getClosestDistance points = minimum $ map (\ [x, y] -> roundTwoDigButWithMagic $ distance x y) $ filter ((==2) . length) $ subsequences points