aplicaFunc :: [a->a] -> a -> a
aplicaFunc [] x = x
aplicaFunc (x:xs) a = aplicaFunc xs (x a)

aplicaFuncoes :: [a->a] -> [a] -> [a]
aplicaFuncoes as [] = [] 
aplicaFuncoes xs (y:ys) = [aplicaFunc xs y] ++ aplicaFuncoes xs ys 
