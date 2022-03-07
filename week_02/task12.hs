main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

findSum :: Int -> Int -> Int -> Int
findSum a b n
  | n <= 3 = error "n must be bigger than 3"
  | otherwise = helper a b n 0
  where
      