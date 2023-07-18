-- Tree definition 
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = (quickSort [lt | lt <- xs, lt <= x]) ++ x:(quickSort [gt | gt <- xs, gt > x]) 

listToBinaryTree :: (Ord a) => [a] -> Tree a
listToBinaryTree [] = Leaf
listToBinaryTree (x:xs) = Node x 
  (listToBinaryTree [lt | lt <- xs, lt <= x]) 
  (listToBinaryTree [gt | gt <- xs, gt > x])
  
binaryTreeToList :: (Ord a) => Tree a -> [a]
binaryTreeToList Leaf = []
binaryTreeToList (Node h lc gc) = (binaryTreeToList lc) ++ h:(binaryTreeToList gc)
  
binaryTreeLookup :: (Ord a) => Tree a -> a -> Bool
binaryTreeLookup Leaf x = False
binaryTreeLookup (Node x' lc gc) x
  | (x == x') = True
  | (x < x') = binaryTreeLookup lc x
  | otherwise = binaryTreeLookup gc x
  
preservesElements :: (Ord a) =>  Tree a -> [a] -> Bool
preservesElements tr xs = all (binaryTreeLookup tr) xs
