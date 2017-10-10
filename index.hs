index :: Int -> [Int] -> [Int]
index x xs = [b | (a,b)<-zip xs [1..(length xs+1)] , a == x]
