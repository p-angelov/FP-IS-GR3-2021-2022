main :: IO()
main = do
    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565

    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565

sumUnevenLC :: Int -> Int -> Int
sumUnevenLC x y = sum [n | n <- [min x y .. max x y], odd n]

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF x y = sum (filter (\n -> odd n) [min x y .. max x y])


