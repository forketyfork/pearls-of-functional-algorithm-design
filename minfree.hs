import Data.List
import Data.Array
import Data.Array.ST

minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

minfreeArray :: [Int] -> Int
minfreeArray = search . checklistArray

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklistArray :: [Int] -> Array Int Bool
checklistArray xs = accumArray (||) False (0,n) 
        (zip (filter (<= n) xs) (repeat True))
    where n = length xs

minfreeMonad :: [Int] -> Int
minfreeMonad = search . checklistMonad

checklistMonad :: [Int] -> Array Int Bool
checklistMonad xs = runSTArray(do
    { a <- newArray (0,n) False;
      sequence [writeArray a x True | x <- xs, x <= n];
      return a })
    where n = length xs

sort' :: [Int] -> [Int]
sort' xs = concat [replicate k x | (x, k) <- assocs $ countlist xs]

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0,n) (zip xs (repeat 1))
    where n = length xs
