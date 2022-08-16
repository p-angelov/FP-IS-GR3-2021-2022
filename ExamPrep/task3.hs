import Data.Char
import Data.List

main :: IO()
main = do
    print $ cook [ApplePie, ApplePie, Burger, Chicken, Chicken, ApplePie] == [Sunny, Rainy, Rainy, Sunny, Rainy]
    print $ cook [ApplePie, Burger, Chicken, Chicken, ApplePie, Burger] == [Rainy,Rainy,Sunny,Rainy,Rainy]

data Food = ApplePie | Burger | Chicken 
 deriving (Show, Eq)
data Wheather = Sunny | Rainy
 deriving (Show, Eq)

cook :: [Food] -> [Wheather]
cook xs = [if x == y then Sunny else Rainy | (x, y) <- zip xs (tail xs)]