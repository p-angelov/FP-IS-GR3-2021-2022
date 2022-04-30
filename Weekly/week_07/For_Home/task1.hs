import Data.Char
import Data.List

main :: IO()
main = do
    print $ getTotal db == 31.2

    print $ getAverage db == 4.457142857142857

    -- print $ getNeeded 750 == [("cheese",750,5.0),("water",500,0.5),("soap",250,4.5)]

    print $ cheapestAlternative "lamb" 5.50 == 0
    print $ cheapestAlternative "milk" 5.00 == 1

type Product = (String, Int, Double)
type Shop = [Product]

db :: Shop
db = [("bread", 1000, 1.20), ("milk", 2000, 4.5), ("lamb", 5000, 10), ("cheese", 750, 5), ("butter", 1000, 5.50), ("water", 500, 0.50), ("soap", 250, 4.50)]

getTotal :: Shop -> Double
getTotal db = sum [price | (_, _, price) <- db]

getAverage :: Shop -> Double
getAverage db = sum [price | (_, _, price) <- db] / (fromIntegral $ length [price | (_, _, price) <- db])

-- getNeeded :: Int -> Shop
-- getNeeded quantity = [q | (_, q, _) <- db, q <= quantity]

cheapestAlternative :: String -> Double -> Int
cheapestAlternative name price = (length [p | (n, _, p) <- db, n /= name || p < price]) - 6

buy :: String -> Int -> Shop -> Shop
buy name quantity db = 