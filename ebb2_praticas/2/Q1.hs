seqCollat :: Int -> [Int]
seqCollat x | x <= 0 = [0]
            | x == 1 = [1]
            | mod x 2 == 0 = 1 : seqCollat (x `div` 2)
            | otherwise = 1 : seqCollat (3*x + 1)

collatz :: Int -> Int
collatz x = foldr1 (+) (seqCollat x)

maior :: [Int] -> Int
maior [] = 0
maior (a:as) | (filter (>a) as) == [] = a
             | otherwise = maior as

maiorSeqCollatz :: [Int] -> Int
maiorSeqCollatz [] = 0
maiorSeqCollatz a = maior mapped
                  where mapped = map collatz a


