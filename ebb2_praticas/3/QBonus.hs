--Bonus
data Tree = Null | Leaf Int | Node Int (Tree) (Tree) deriving Show
 
insert1 :: Tree -> Tree -> Tree 
insert1 x (Null) = x 
insert1 (Null) x = x
insert1 (Leaf a) (Leaf b) | a > b = (Node a (Leaf b) (Null)) 
                          | otherwise = (Node a (Null) (Leaf b))
insert1 (Node a l r) (Leaf b) | a > b = Node b (Null) (Node a l r) 
                              | otherwise =  Node b (Node a l r) (Null)
insert1 (Leaf a) (Node b c d) | a > b = (Node b c (insert1 (Leaf a) d))
                              | otherwise = (Node b (insert1 (Leaf a) c) d)
insert1 (Node a b c) (Node x y z) | a > x = (Node x y (insert1 (Node a b c) z))
                                  | otherwise = (Node x (insert1 (Node a b c) y) z)

delete :: Int -> Tree -> Tree
delete x (Null) = (Null)
delete x (Leaf y) | x == y = (Null)
                  | otherwise = (Leaf y) 
delete x (Node a l r) | x == a = (insert1 r l)
                      | x > a = (Node a l (delete x r))
                      | otherwise = (Node a (delete x l) r)

arvLista :: Tree -> [Int]
arvLista (Null) = []
arvLista (Leaf a) = [a]
arvLista (Node a b c) = (arvLista b) ++ [a] ++ (arvLista c)

listArv :: [Int] -> Tree
listArv [] = Null
listArv [x] = (Leaf x)
listArv (a:as) = insert1 (Leaf a) (listArv as)

somaArv :: Tree -> Int
somaArv (Null) = 0
somaArv (Leaf a) = a
somaArv (Node a b c) = (somaArv b) + a + (somaArv c)

maiorArv :: Tree -> Int
maiorArv (Null) = 0
maiorArv (Leaf a) = 1
maiorArv (Node a b c) = 1 + (max1 (maiorArv b) (maiorArv c))

max1 :: Int -> Int -> Int
max1 a b | a > b = a
         | otherwise = b
