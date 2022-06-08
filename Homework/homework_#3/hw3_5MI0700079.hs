main :: IO()
main = do

    print $ rf (Song "Mozart""The Marriage of Figaro Overture" 270) == "Summertime"
    print $ rf (Song "Gershwin""Summertime" 300) == "Rhapsody in Blue"
    print $ rf (Song "Gershwin""Rhapsody in Blue" 1100) == "Rhapsody in Blue"

songs = [(Song "Mozart""The Marriage of Figaro Overture" 270), (Song "Gershwin""Summertime" 300), (Song "Queen""Bohemian Rhapsody" 355), (Song "Gershwin""Rhapsody in Blue" 1100)]

rf = recommender (Playlist songs)

recommender :: Playlist -> (Song -> SongName)
recommender (Playlist pl) = (\name -> getsNextSong name)
 where
     getsNextSong :: Song -> SongName
     getsNextSong name
      |null $ songsFromAuthor name = helper name
      |otherwise = nextSongSameAuthor name
     
     helper :: Song -> SongName
     helper name 
      |null $ songs name = getSongName name
      |otherwise = nextSongDifferentAuthor name
     
     getSongName :: Song -> SongName
     getSongName (Song author name length) = name

     songs :: Song -> [Song]
     songs name = tail $ dropWhile (\name' -> name' /= name) pl    

     songsFromAuthor :: Song -> [SongName]
     songsFromAuthor (Song author name length) = map (\(Song author' name' length') -> name') $ filter (\(Song author' name' length') -> author' == author) $ songs (Song author name length)
     
     nextSongSameAuthor :: Song -> SongName
     nextSongSameAuthor (Song author name length) = head $ songsFromAuthor (Song author name length)
     
     nextSongDifferentAuthor :: Song -> SongName
     nextSongDifferentAuthor name = head $ map(\(Song author' name' length') -> name') $ songs name

type AuthorName = String
type SongName = String
type SongLength = Int

data Song = Song AuthorName SongName SongLength deriving (Eq)
data Playlist = Playlist [Song]