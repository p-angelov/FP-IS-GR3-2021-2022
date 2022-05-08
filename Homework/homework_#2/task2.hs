import Data.Char
import Data.List

main :: IO()
main = do
    print $ actorsNetworthHigherThan 124000000 db == [([["Billy Bob Thornton","Scarlett Johansson"],["Kim Basinger","Alec Baldwin","Harrison Ford"],["Harrison Ford"]],["The Man Who Wasn't There","Star Wars","Empire Strikes Back"],"George Lucas",200000000),([["Liv Tyler"]],["Logan's run"],"Ted Turner",125000000)]
    print $ actorsNetworthHigherThan 125000000 db == [([["Billy Bob Thornton","Scarlett Johansson"],["Kim Basinger","Alec Baldwin","Harrison Ford"],["Harrison Ford"]],["The Man Who Wasn't There","Star Wars","Empire Strikes Back"],"George Lucas",200000000)]

actorsNetworthHigherThan :: Integer -> MovieDB -> [([[Name]], [Title], Name, Integer)]
actorsNetworthHigherThan k (ms, ss, ssI, std, mE) = map (\(name, id, nw) -> (actorsFromME id, takeMovieFromID id, name, nw ) ) allMovieExecsHigherThanK -- основна функция
 where
     takeMovieFromID :: ProducerID -> [Title]
     takeMovieFromID id = map (\(title, _, _, _, _) -> title) $ filter (\(_, _, _, _, id') -> id' == id) ms -- функция, която връща филми по подадено id на продуцент

     actorsInMovie :: Title -> [Name]
     actorsInMovie title = map (\(actor, _) -> actor) $ filter (\(_, filmT) -> filmT == title) ssI -- функция извеждаща актьорите в даден филм

     actorsFromME :: ProducerID -> [[Name]]
     actorsFromME id = map (\title -> actorsInMovie title) $ takeMovieFromID id -- функция, която връща актьорите по подадено id на продуцент

     allMovieExecsHigherThanK :: [MovieExec]
     allMovieExecsHigherThanK = filter (\(_, _, nw) -> nw > k) mE -- функция филтрираща продуцентите със сътояние по-голямо от k

type Name = String
type Title = String
type Address = String
type Year = Int
type Gender = Char
type Length = Int
type ProducerID = Int
type Networth = Integer

type Movie = (Title, Year, Length, Name, ProducerID)
type MovieStar = (Name, Gender)
type StarsIn = (Name, Title)
type Studio = (Name, Int)
type MovieExec = (Name, ProducerID, Networth)

type MovieDB = ([Movie], [MovieStar], [StarsIn], [Studio], [MovieExec])

studios :: [Studio]
studios = [("Disney", 199),("USA Entertainm.", 222),("Fox", 333),("Paramount", 123),("MGM", 555)]

movieExecs :: [MovieExec]
movieExecs = [("George Lucas", 555, 200000000),("Ted Turner", 333, 125000000),("Stephen Spielberg", 222, 100000000),("Merv Griffin", 199, 112000000),("Calvin Coolidge", 123, 20000000)]

movies :: [Movie]
movies = [("Pretty Woman", 1990, 119, "Disney", 199),("The Man Who Wasn't There", 2001, 116, "USA Entertainm.", 555),("Logan's run", 1976, 120, "Fox", 333),("Star Wars", 1977, 124, "Fox", 555),("Empire Strikes Back", 1980, 111, "Fox", 555),("Star Trek", 1979, 132, "Paramount", 222),("Star Trek: Nemesis", 2002, 116, "Paramount", 123),("Terms of Endearment", 1983, 132, "MGM", 123),("The Usual Suspects", 1995, 106, "MGM", 199),("Gone With the Wind", 1938, 238, "MGM", 123),("The Fellowship of the Ring", 2001, 178, "USA Entertainm.", 222)]

stars :: [MovieStar]
stars = [("Jane Fonda", 'F'),("Alec Baldwin", 'M'),("Kim Basinger", 'F'),("Harrison Ford", 'M'),("Debra Winger", 'F'),("Jack Nicholson", 'M'),("Sandra Bullock", 'F'),("Orlando Bloom", 'M'),("Cate Blanchett", 'F'),("Liv Tyler", 'F'),("Billy Bob Thornton", 'M'),("Scarlett Johansson", 'F')]

starsIn :: [StarsIn]
starsIn = [("Kim Basinger", "Star Wars"),("Alec Baldwin", "Star Wars"),("Harrison Ford", "Star Wars"),("Harrison Ford", "Empire Strikes Back"),("Jack Nicholson", "The Usual Suspects"),("Jane Fonda", "Terms of Endearment"),("Jack Nicholson", "Terms of Endearment"),("Sandra Bulloc", "The Usual Suspects"),("Billy Bob Thornton", "The Man Who Wasn't There"),("Scarlett Johansson", "The Man Who Wasn't There"),("Orlando Bloom", "The Fellowship of the Ring"),("Cate Blanchett", "The Fellowship of the Ring"),("Liv Tyler", "Logan's run")]

db :: MovieDB
db = (movies, stars, starsIn, studios, movieExecs)

