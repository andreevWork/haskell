module Algoritms.List.ListSort (quickSort) where

quickSort :: (Ord a) => [a] -> [a]

quickSort [] = []
quickSort (h:xs) =
    let lower = quickSort [x | x <- xs, x <= h]
        bigger = quickSort [x | x <- xs, x > h]
    in lower ++ [h] ++  bigger
