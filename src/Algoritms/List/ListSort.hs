module Algoritms.List.ListSort (
    quickSort,
    mergeSort
) where

quickSort :: (Ord a) => [a] -> [a]

quickSort [] = []
quickSort (h:xs) =
    let lower = quickSort [x | x <- xs, x <= h]
        bigger = quickSort [x | x <- xs, x > h]
    in lower ++ [h] ++  bigger


mergeSort :: (Ord a) => [a] -> [a]

mergeSort [] = []
mergeSort xs
    | middle == 0 = xs
    | otherwise = mergeSort' (mergeSort $ take middle xs) (mergeSort $ drop middle xs)
    where middle = (length xs) `div` 2

mergeSort' :: (Ord a) => [a] -> [a] -> [a]

mergeSort' [] xs = xs
mergeSort' xs [] = xs
mergeSort' list_xs@(h:xs) list_ys@(g:ys)
    | h <= g = [h] ++ mergeSort' xs list_ys
    | g < h = [g] ++ mergeSort' list_xs ys
