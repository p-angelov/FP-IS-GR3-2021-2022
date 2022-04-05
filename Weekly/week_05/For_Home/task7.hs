import Data.Char
import Data.List

main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"

repeater :: [Char] -> (Int -> String -> String)
repeater str = (\ count glue -> if count == 1 then str else str ++ glue ++ (repeater str) (count - 1) glue)