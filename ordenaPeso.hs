ordenaPeso :: (a -> Int) -> [a] -> [a]
ordenaPeso f (a:as) = ordenaPeso f [y | y<-as , f y < f a] ++ [a] ++ ordenaPeso f [x | x <- as, f x >= f a]
 
