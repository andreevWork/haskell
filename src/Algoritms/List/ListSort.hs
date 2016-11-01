module Algoritms.List.ListSort (
    quickSort,
    heapSort,
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



heapSort :: (Ord a) => [a] -> [a]

heapSort [] = []
heapSort xs
    | length xs == 1 = xs
    | otherwise = heapSort remaining_heaps ++ [top_heap]
    where (top_heap:remaining_heaps) = buildHeap xs


buildHeap :: (Ord a) => [a] -> [a]

buildHeap heap =
    let last_heap_index = (length heap) `div` 2 - 1
        buildHeap' heap index
            | index == 0 = new_heap
            | otherwise = buildHeap' new_heap (index - 1)
            where new_heap = siftingDownElement heap index
    in buildHeap' heap last_heap_index


siftingDownElement :: (Ord a) => [a] -> Int -> [a]

siftingDownElement [] _ = []
siftingDownElement heap element_index
    | left_index > last_index = heap
    | left_index == last_index && need_swap = switchElements heap element_index last_index
    | need_swap = siftingDownElement (switchElements heap element_index max_index) max_index
    | otherwise = heap
    where last_index = length heap - 1
          left_index = 2 * element_index + 1
          right_index = 2 * element_index + 2
          heap_element = heap!!element_index
          (max_child, max_index) = getMaxElementWithIndex heap left_index right_index
          need_swap = heap_element < max_child


type Index = Int

getMaxElementWithIndex :: (Ord a) => [a] -> Index -> Index -> (a, Index)

getMaxElementWithIndex xs i1 i2
    | i2 > last_index = (el1, i1)
    | el1 >= el2 = (el1, i1)
    | el1 < el2 = (el2, i2)
    where last_index = length xs - 1
          el1 = xs!!i1
          el2 = xs!!i2

switchElements :: [a] -> Index -> Index -> [a]

switchElements [] _ _ = []
switchElements xs i1 i2 =
    let start = take i1 xs
        (el1:middle) = drop i1 (take i2 xs)
        (el2:end) = drop i2 xs
    in start ++ [el2] ++ middle ++ [el1] ++ end