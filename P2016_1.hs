--Questao 1


type Chave = [(Char,Char)]

rot13parcial :: Chave 
rot13parcial = [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),
 ('g','t'),('h','u'),('i','v'),('j','w'),('k','x'),('l','y'), ('m','z')]

existE :: Chave -> Char -> Bool
existE [] _ = False
existE ((a,b):cs) x | x == a = True
		    | otherwise = existE cs x 

equivalente :: Chave -> Char -> [Char]
equivalente cs a = [y | (x,y) <- cs , z <- [a], z == x]

cipher :: Chave -> String -> String
cipher _ [a] = [a]
cipher cs (x:xs) | existE cs x = equivalente cs x ++ cipher cs xs
		 | otherwise = x:(cipher cs xs)

--Questao 2

inverteChave :: Chave -> Chave
inverteChave cs = [(y,x) | (x,y) <- cs]



--Questao 5
data KeyTree = Node Char Char KeyTree KeyTree | Empty deriving(Show)
chaveParcial :: KeyTree
chaveParcial = Node 'h' 'u' (Node 'c' 'p' (Node 'b' 'o' (Node 'a' 'n' Empty Empty) Empty)(Node 'e' 'r' Empty Empty)) (Node 'l' 'y' Empty (Node 'm' 'z' Empty Empty))

checkT :: KeyTree -> Char -> Bool
checkT Empty _ = False
checkT (Node a b c d) ch | ch == a = True
			 | otherwise = (checkT c ch) || (checkT d ch)

searchT :: KeyTree -> Char -> [Char]
searchT Empty _ = []
searchT (Node a b c d) ch | a == ch = [b]
			  | otherwise = (searchT c ch) ++ (searchT d ch)


cipherT :: KeyTree -> String -> String
cipherT _ [] = []
cipherT tr (x:xs) | checkT tr x = (searchT tr x) ++ cipherT tr xs
		  | otherwise = x:(cipherT tr xs) 
