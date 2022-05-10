import Data.Char
import Data.List

main :: IO()
main = do
    print $ getAverageBalance (accounts1, people1) (\(_,_,city) -> city == "Burgas") == 24.95
    print $ getAverageBalance (accounts1, people1) (\(_,(n:_),_) -> n == 'P') == 18.85
    print $ getAverageBalance (accounts1, people1) (\ (n,_,_) -> n == 2) == 26.8

    print $ averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Burgas","Plovdiv"] == 23.62
    print $ averageBalanceOfCities (accounts1,people1) ["Pleven", "Burgas", "Sofia","Gabrovo","Stara Zagora"] == 39.25
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia", "Gabrovo", "Burgas"] == 39.25

type Account = (Int, Int, Double)
type Person = (Int, String, String)

people1 :: [Person]
people1 = [(1, "Ivan", "Sofia"),(2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"),(4, "Petya", "Burgas")]

accounts1 :: [Account]
accounts1 = [(1, 1, 12.5),(2, 1, 123.2),(3, 2, 13.0),(4, 2, 50.2),(5, 2, 17.2),(6, 3, 18.3),(7, 4, 19.4)]

-- A)
rounder :: Double -> Double
rounder num = (fromIntegral . round $ num * f) / f
  where f = 100

getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance (accounts1, people1) p = rounder (sum balance / (fromIntegral $ length balance))
  where
      balance = concat [ get id | person@(id, _, _) <- people1, p person]
      get id = [ b | (_, idP, b) <- accounts1, idP == id ]

-- B)
averageBalanceOfCities :: ([Account], [Person]) -> [String] -> Double
averageBalanceOfCities (accounts1, people1) cities = getAverageBalance (accounts1, people1) (\(_,_,city) -> elem city cities)