doisAdois :: Int -> [(Int,Int)]
doisAdois x = [(a,y) | a <- [0..x], y<-[0..x], a <= y ] 
