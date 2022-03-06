main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not

truncatablePrime :: Int -> Bool
truncatablePrime m