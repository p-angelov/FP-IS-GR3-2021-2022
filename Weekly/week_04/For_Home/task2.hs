main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs
  | xs == [] = False
  | head xs == n = True
  | otherwise = isPresentRecNonPM n (tail xs)

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM a [] = False
isPresentRecPM a (x:xs) = a == x || isPresentRecPM a xs

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc = elem
