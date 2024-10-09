import Data.Array
import Data.List

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) 
    (zip (filter (<= n) xs) (repeat True))
    where n = length xs


findFirstAbsent :: [Int] -> Int
findFirstAbsent = length . (takeWhile id) . elems . checklist