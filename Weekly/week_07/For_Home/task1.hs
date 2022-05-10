import Data.Char
import Data.List

main :: IO()
main = do
    print $ getTotal db == 31.2

    print $ getAverage db == 4.457142857142857

    print $ getNeeded 750 == [("cheese",750,5.0),("water",500,0.5),("soap",250,4.5)]

    print $ cheapestAlternative "lamb" 5.50 == 0
    print $ cheapestAlternative "milk" 5.00 == 1

    print $ buy "bread" 500 db == [("bread",500,1.2),("milk",2000,4.5),("lamb",5000,10.0),("cheese",750,5.0),("butter",1000,5.5),("water",500,0.5),("soap",250,4.5)]
    print $ (buy "water" 500 $ buy "bread" 500 db) == [("bread",500,1.2),("milk",2000,4.5),("lamb",5000,10.0),("cheese",750,5.0),("butter",1000,5.5),("soap",250,4.5)]
    print $ (buy "soap" 100 $ buy "water" 500 $ buy "bread" 500 db) == [("bread",500,1.2),("milk",2000,4.5),("lamb",5000,10.0),("cheese",750,5.0),("butter",1000,5.5),("soap",150,4.5)]
    print $ (buy "bread" 500 $ buy "soap" 100 $ buy "water" 500 $ buy "bread" 500 db) == [("milk",2000,4.5),("lamb",5000,10.0),("cheese",750,5.0),("butter",1000,5.5),("soap",150,4.5)]
    -- print $ (buy "water" 100 $ buy "bread" 500 $ buy "soap" 100 $ buy "water" 500 $ buy "bread" 500 db) -- error: Out of stock!
    -- print $ (buy "soap" 200 $ buy "bread" 500 $ buy "soap" 100 $ buy "water" 500 $ buy "bread" 500 db) -- error: Out of stock!

type Product = (String, Int, Double)
type Shop = [Product]

db :: Shop
db = [("bread", 1000, 1.20), ("milk", 2000, 4.5), ("lamb", 5000, 10), ("cheese", 750, 5), ("butter", 1000, 5.50), ("water", 500, 0.50), ("soap", 250, 4.50)]

getTotal :: Shop -> Double
getTotal db = sum [price | (_, _, price) <- db]

getAverage :: Shop -> Double
getAverage db = sum [price | (_, _, price) <- db] / (fromIntegral $ length [price | (_, _, price) <- db])

getNeeded :: Int -> Shop
getNeeded quantity = filter (\(_, q, _) -> q <= quantity) db

cheapestAlternative :: String -> Double -> Int
cheapestAlternative name price = (length [p | (n, _, p) <- db, n /= name || p < price]) - 6

buy :: String -> Int -> Shop -> Shop
buy name quantity [] = error "Not available!"
buy name quantity ((n, q, price):xs)
  | name == n && quantity > q = error "Out of stock!"
  | name == n && quantity == q = xs
  | name == n && q > quantity = (name, q - quantity, price):xs
  | otherwise = (n, q, price) : buy name quantity xs