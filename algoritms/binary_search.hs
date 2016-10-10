binary_search :: Ord a => [a] -> a -> Bool

binary_search [] _ = False
binary_search xs elem
    | middle == 0 = elem == xs!!0
    | elem >= xs!!middle = binary_search (drop middle xs) elem
    | elem < xs!!middle = binary_search (take middle xs) elem
    where middle = length xs `div` 2
