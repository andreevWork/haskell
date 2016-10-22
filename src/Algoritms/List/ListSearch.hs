module Algoritms.List.ListSearch (binarySearch) where

binarySearch :: Ord a => [a] -> a -> Maybe Int

binarySearch [] _ = Nothing
binarySearch xs elem =
    let binarySearch' xs elem index
               | elem == xs!!middle = Just (index + middle)
               | middle == 0 = Nothing
               | elem > xs!!middle = binarySearch' (drop middle xs) elem $ index + middle
               | elem < xs!!middle = binarySearch' (take middle xs) elem index
               where middle = (length xs) `div` 2
    in binarySearch' xs elem 0
