import Data.List


makeList :: [Int] -> [Int]
makeList as =[ as !! y | (x,y) <- zip as [0..((length as) - 1)], z <- take 1  (elemIndices x as), z /= y ]

