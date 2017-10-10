tuplaQuant :: [Int] -> [(Int,Int)]
tuplaQuant [] = []
tuplaQuant x = remove ( organize (quickSort x) (quickSort x))

checkHowMany :: [Int] -> Int -> Int
checkHowMany [] _ = 0
checkHowMany (a:as) x | x == a = 1 + checkHowMany as x
                      | otherwise = checkHowMany as x

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:as) = quickSort [a| a <- as, a <= x] ++ [x] ++ quickSort [c| c <- as, c > x]

organize :: [Int] -> [Int] -> [(Int,Int)]
organize [] _ = []
organize (a:as) b = ((a, checkHowMany b a)) : organize as b

remove :: [(Int,Int)] -> [(Int,Int)]
remove x | x == [] = []
         | (drop 1 x) == [] = x
         | head x == head (drop 1 x) = remove (drop 1 x)
         | otherwise = head x : remove (drop 1 x)