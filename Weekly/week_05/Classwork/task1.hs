import Data.List
main :: IO()
main = do
    print $ (myLambda (\ x -> x)) 5 == 5
    print $ (myLambda (\ x -> x)) "Hello" == "Hello"
    print $ (myLambda (+1)) 5 == 6

    print $ (negatePred (\x -> mod x 2 == 0)) 2 == False

    print $ (compose (\x -> x - 5) (\y -> y + 25)) 5 == 25
    print $ (compose group sort) "Hello World" == [" ","H","W","d","e","lll","oo","r"]

    print $ (partiallyApply (\x y -> 10 * x + y) 5) 10 == 60

myLambda :: (a -> a) -> (a -> a)
myLambda f = (\ x -> f x)
-- myLambda f = f

negatePred :: (a -> Bool) -> (a -> Bool)
negatePred p = (\ x -> not (p x))
-- negatePred p = not . p

compose :: (c -> a) -> (b -> c) -> (b -> a)
compose f g = f . g

partiallyApply :: (a -> a -> a) -> a -> (a -> a)
partiallyApply f x = f x
-- partiallyApply f x = (\ y -> f x y)