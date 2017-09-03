

palindromo :: String -> Bool
palindromo as | as == reverse as = True
	      | otherwise = False  
