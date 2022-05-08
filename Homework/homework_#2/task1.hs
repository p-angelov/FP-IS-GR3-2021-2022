import Data.Char
import Data.List

main :: IO()
main = do
    
    print $ sunkK 0 database == [("Germany",0),("Gt.Britain",0)]
    print $ sunkK 2 database == [("Germany",0),("Japan",2),("Gt.Britain",0)]
    print $ sunkK 9 database == [("Germany",0),("USA",4),("Japan",2),("Gt.Britain",0)]
    print $ sunkK 20 database == [("Germany",0),("USA",4),("Japan",2),("Gt.Britain",0)]

sunkK :: Int -> Database -> [(Name, Int)]
sunkK k (os, bs, ss, cs) = filter (\x -> snd x <= k) $ map (\c -> (c, counter c countries)) allCountries --основна функция
 where
     intoClasses :: [Class]
     intoClasses = [ class' | (name, class', _) <- ss, elem name sunkenShips ]

     intoCountries :: Class -> Name
     intoCountries c = head [ country | (shipClass, _, country) <- cs, shipClass == c ]

     countries :: [Name]
     countries = map (\x -> intoCountries x) intoClasses

     sunkenShips :: [Name]
     sunkenShips = map (\(name, _, _) -> name) $ filter (\(_, _, res) -> res == "sunk") os -- всички потънали кораби в базата данни

     allCountries :: [Name]
     allCountries = nub $ map (\(_, _, countryName) -> countryName) cs -- списък с всички държави в базата данни

     counter :: Ord a => a -> [a] -> Int -- универсална помощна функция, използвана в основната, преброяваща колко елемента на xs удовлетворяват
     counter _ [] = 0                    -- (\y-> y == x)
     counter x xs = length $ filter (\y-> y == x) xs

type Name = String
type Date = String
type Class = String
type Type = String
type Result = String
type Launched = Int

type Battle = (Name, Date)
type Ship = (Name, Class, Launched)
type Outcome = (Name, Name, Result)
type ShipClass = (Name, Type, Name)

type Database = ([Outcome], [Battle], [Ship], [ShipClass])

outcomes :: [Outcome]
outcomes = [ ("Bismarck", "North Atlantic", "sunk"), ("California", "Surigao Strait", "ok"), ("Duke of York", "North Cape", "ok"), ("Fuso", "Surigao Strait", "sunk"), ("Hood", "North Atlantic", "sunk"), ("King George V", "North Atlantic", "ok"), ("Kirishima", "Guadalcanal", "sunk"), ("Prince of Wales", "North Atlantic", "damaged"), ("Rodney", "North Atlantic", "ok"), ("Schamhorst", "North Cape", "sunk"), ("South Dakota", "Guadalcanal", "damaged"), ("Tennessee", "Surigao Strait", "ok"), ("Washington", "Guadalcanal", "ok"), ("Prince of Wales", "Guadalcanal", "ok"), ("West Virginia", "Surigao Strait", "ok"), ("Yamashiro", "Surigao Strait", "sunk"), ("California", "Guadalcanal", "damaged") ]

battles :: [Battle]
battles = [ ("Guadalcanal", "1942-11-15"), ("North Atlantic", "1941-05-25"), ("North Cape", "1943-12-26"), ("Surigao Strait", "1944-10-25") ]

ships :: [Ship]
ships = [ ("California", "Tennessee", 1921), ("Haruna", "Kongo", 1916), ("Hiei", "Kongo", 1914), ("Iowa", "Iowa", 1943), ("Kirishima", "Kongo", 1915), ("Kongo", "Kongo", 1913), ("Missouri", "Iowa", 1944), ("Musashi", "Yamato", 1942), ("New Jersey", "Iowa", 1943), ("North Carolina", "North Carolina", 1941), ("Ramillies", "Revenge", 1917), ("Renown", "Renown", 1916), ("Repulse", "Renown", 1916), ("Resolution", "Renown", 1916), ("Revenge", "Revenge", 1916), ("Royal Oak", "Revenge", 1916), ("Royal Sovereign", "Revenge", 1916), ("Tennessee", "Tennessee", 1920), ("Washington", "North Carolina", 1941), ("Wisconsin", "Iowa", 1944), ("Yamato", "Yamato", 1941), ("Yamashiro", "Yamato", 1947), ("South Dakota", "North Carolina", 1941), ("Bismarck", "North Carolina", 1911), ("Duke of York", "Renown", 1916), ("Fuso", "Iowa", 1940), ("Hood", "Iowa", 1942), ("Rodney", "Yamato", 1915), ("Yanashiro", "Yamato", 1918), ("Schamhorst", "North Carolina", 1917), ("Prince of Wales", "North Carolina", 1937), ("King George V", "Iowa", 1942), ("West Virginia", "Iowa", 1942) ]

classes :: [ShipClass]
classes = [("Bismarck", "bb", "Germany"), ("Iowa", "bb", "USA"), ("Kongo", "bc", "Japan"), ("North Carolina", "bb", "USA"), ("Renown", "bc", "Gt.Britain"), ("Revenge", "bb", "Gt.Britain"), ("Tennessee", "bb", "USA"), ("Yamato", "bb", "Japan")]

database :: Database
database = (outcomes, battles, ships, classes)