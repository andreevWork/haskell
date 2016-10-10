module ListSearch (quick_sort) where

quick_sort :: Ord a => [a] -> [a]

quick_sort [] = []
quick_sort (h:xs) =
    let lower = quick_sort [x | x <- xs, x <= h]
        bigger = quick_sort [x | x <- xs, x > h]
    in lower ++ [h] ++  bigger
