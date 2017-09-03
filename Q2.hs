import Data.List

countL :: Int -> Int -> [Int] -> (Int,Int)
countL x c [] = (x,c)
countL x c (a:as) | a /= x = countL x c as
	       | otherwise = countL x (c+1) as 

tuplasC :: [Int] -> [(Int,Int)]
tuplasC as = [y | x <- sort as, y <- [countL x 0 as]] 

repOut :: [(Int,Int)] -> [(Int,Int)]
repOut [] = []
repOut (a:as) | notElem a as  = [a] ++ repOut as
	      | otherwise = repOut as

tuplasF :: [Int] -> [(Int,Int)]
tuplasF x = repOut(tuplasC x)
