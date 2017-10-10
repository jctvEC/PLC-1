reduz1 :: [Int] -> [Int]
reduz1 x = organize (zip x [0..(length x)])

organize :: [(Int,Int)] -> [Int]
organize y = [a | (a,b) <- y, elem b (indices y a)]
           where indices y a = drop 1 [k | (c,k) <- y, c == a]