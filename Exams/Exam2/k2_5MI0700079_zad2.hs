import Data.Char
import Data.List

main :: IO()
main = do
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False

cP = canPass (1,2)

type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)

data Attendance = Absent | Late | Present
type StudentRecord = [Attendance]

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (misses, lates) = helper 0 0 
 where
     helper :: Int -> Int -> StudentRecord -> Bool
     helper m l (x1:x2:xs)
      | l > lates = False
      | x1 == Absent = helper m (l + 1) (x2:xs)