{-# LANGUAGE BangPatterns #-}

data Tree a =  EmptyNode | Node a (Tree a) (Tree a)
    deriving (Show)

instance Eq a => Eq (Tree a) where
    (Node a _ _) == (Node b _ _) = a == b


createTree :: [a] -> Tree a

createTree [] = EmptyNode
createTree (x:y:xs) = Node x 