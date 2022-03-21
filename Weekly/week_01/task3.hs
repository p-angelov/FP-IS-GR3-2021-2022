main :: IO()
main = do
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5

sqAvg :: Int -> Int -> Double
sqAvg a b = (fromIntegral a ^ 2 + fromIntegral b ^ 2) / 2