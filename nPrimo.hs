confPrimo :: Int -> Int -> Bool
confPrimo 0 _ = False
confPrimo 1 _ = False
confPrimo a b | a==b = True
	      | (a `mod` b) == 0 && (b > 1) && (b<a) = False
	      | otherwise = confPrimo a (b+1)

primo :: Int -> Int -> Int -> Int
primo x y z | confPrimo y 1 == True && x == z = y
	    | confPrimo y 1 && x /= z = primo x (y+1) (z+1)
	    | confPrimo y 1 /= True && x /= z = primo x (y+1) z


nPrimo :: Int -> Int
nPrimo x = primo x 2 0		
