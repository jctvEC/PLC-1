primo :: Integer -> Bool 
primo 1 = False 
primo x | (length [ y | y <- [1..x], x`mod`y == 0, y /=x ]) > 0 = False 
	| otherwise = True 
