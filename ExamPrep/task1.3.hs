import Data.Char
import Data.List

main :: IO()
main = do
    -- print $ getCriticalBalance db fromVarna 10 -- == [(3,5.0)]
    -- print $ getCriticalBalance db (not . fromVarna) 15 -- == [(2,4.0), (4,12.0)]
    print $ printPerson db

db = ([(1, 1, 10), (2, 1, 11), (3, 1, 12), (4, 2, 3), (5, 2, 1), (6, 3, 2), (7, 3, 3), (8, 4, 12)], [(1, "Ivan", "Varna"), (2, "Petar", "Burgas"), (3, "Georgi", "Varna"), (4, "Yordan", "Plovdiv")])

getCriticalBalance :: ([Account], [Person]) -> (Person -> Bool) -> Balance -> [(PersonID, Balance)]
getCriticalBalance (acc, person) p s = [ (pId, (sumBalance (acc, person))) | (pId, _, _) <- person, (sumBalance (acc, person)) < s && p (head person)]

sumBalance :: ([Account], [Person]) -> Double
sumBalance (acc, person) = sum [bal | (_, idP, bal) <- acc, (pId, _, _) <- person, idP == pId]

printPerson :: ([Account], [Person]) -> [String]
printPerson (acc, person) = [name | (_, name, _) <- person]

type PersonID = Int
type Name = String
type City = String
type AccountID = Int
type Balance = Double
type Person = (PersonID, Name, City)
type Account = (AccountID, PersonID, Balance)

fromVarna :: Person -> Bool
fromVarna (_, _, "Varna") = True
fromVarna _ = False