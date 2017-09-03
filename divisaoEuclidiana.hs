subdiv :: Int -> Int -> Int -> Int
subdiv a b c | a == c = c
	     | a > b = subdiv (a-b) b (c+1)
	     | otherwise = 0


divisaoEuclidiana :: Int -> Int -> (Int,Int) 
divisaoEuclidiana x y = (subdiv x y 0, restEuclides x y)

