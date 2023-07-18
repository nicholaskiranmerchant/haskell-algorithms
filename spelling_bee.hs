--import Data.Random

-- Given
-- dictionary = []
-- board = []

has :: (Eq a) => [a] -> a -> Bool
has (x: xs) y = (||) ((==) x y)  (has xs y)

hasAll :: (Eq a) => [a] -> [a] -> Bool
hasAll xs ys = all (has xs) ys

filterHasAll :: (Eq a) => [a] -> [[a]] -> [[a]]
filterHasAll xs yss = filter (hasAll xs) yss

data Judgement a = Short [a] a | SinCenter [a] a | BadLetter [a] a | Found [a] a | NotInDictionary [a] a | NewWord [a] a

judge :: (Eq a) => Int -> [a] -> [[a]] -> [[a]] -> [a] -> Judgement [a]
judge n (l:ls) d f w
    | (>) n (length w) = Short f w
    | not (has w l) = SinCenter f w
    | not (hasAll ls w) = BadLetter f w
    | has f w = Found f w
    | not (has d w) = NotInDictionary f w
    | otherwise = NewWord f w

filterDictionary :: (Eq a) => Int -> [a] -> [[a]] -> [[a]]
filterDictionary n ls ds = filter valid ds
    where valid d = (&&) ((<=) n (length d)) (hasAll ls d)


board = ["h", "a", "y", "e", "w"]
dict = ["heya", "hoew", "heet", "hawe", "haya"]
spellingBeeJudge = judge 4 board subDict


