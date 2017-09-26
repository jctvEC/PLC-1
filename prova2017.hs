isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (a:as) | as == [] = True	
		| a > head as = False
		| otherwise = isSorted as

compB :: [Int] -> Int -> [Int]
compB (a:as) x | x > a = x:as
	       | otherwise = a:as

bubble :: [Int] -> [Int]
bubble [] = []
bubble [a] = [a]
bubble (a:b:as) | a > b = [b]++(bubble (a:as)) 
	        | otherwise = [a] ++ (bubble (b:as))

sBubble :: [Int] -> [Int]
sBubble s | isSorted s = s
	  | otherwise = sBubble (bubble s)
