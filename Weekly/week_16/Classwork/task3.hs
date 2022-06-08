import Data.Char
import Data.List

main :: IO()
main = do
    print $ cook [ApplePie, ApplePie, Burger, Chicken, Chicken, ApplePie] == [Sunny, Rainy, Rainy, Sunny, Rainy]
    print $ cook [ApplePie, Burger, Chicken, Chicken, ApplePie, Burger] == [Rainy, Rainy, Sunny, Rainy, Rainy]

data Food = ApplePie | Burger | Chicken
 deriving (Eq, Show)

data Weather = Sunny | Rainy
 deriving (Eq, Show)

cook :: [Food] -> [Weather]
cook [] = []
cook [_] = []
cook (x1:x2:xs)
 | x1 == x2 = Sunny : cook (x2:xs)
 | otherwise = Rainy : cook (x2:xs)

cook' :: [Food] -> [Weather]
cook' xs = map (\ (p, r) -> if p == r then Sunny else Rainy) zip xs $ tail xs
