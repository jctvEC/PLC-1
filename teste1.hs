vendas :: Int -> Int
vendas x | x == 0 = 55 | x == 1 = 90 | x == 2 = 10 | x == 3 = 115 | x == 4 = 90 | otherwise = error "venda maior que o esperado"

--mesma coisa que a de cima, pega logo os padrões
{-
vendas' :: Int -> Int
vendas' 0 = 55
vendas' 1 = 90
-}
--iteracoes so com recursao (Int tem precisao nas casas decimais)
totalVendas :: Int -> Int
totalVendas n | n == 0 = vendas 0 | n > 0 = vendas n + totalVendas (n-1)


answer :: Int
answer = 42

maxi :: Int -> Int -> Int
maxi x y | x > y = x | otherwise = y

doubleSmallNumber x = if x > 100
			then x
			else x*2
-- casas decimais infinitas com Integer
fat :: Integer -> Integer
fat 0 = 1
fat n | n > 0 = n * fat (n-1) | otherwise = error "fatorial de numero negativo"

f :: Int -> Int -> Int
f 0 0 = 0
f 0 _ = 99
f 1 y =  200 + y
f x 0 = 20 + x
f x y = x + 2 * y + 9

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = x == y && x ==z

equalCount :: Int -> Int -> Int -> Int
equalCount x y z | allEqual x y z = 3
		 | ((x /= y) && (x /= z) && (y /= z)) = 0
		 | otherwise = 1
sumSquares :: Int -> Int -> Int
{-sumSquares x y = sqX + sqY
	where sqX = x * x
	      sqY = y * y
-}
sumSquares x y = sq x + sq y
	where sq z = z * z

sumSquares' :: Int -> Int -> Int
sumSquares' x y = let sqX = x * x
		      sqY = y * y
		  in sqX + sqY

{- Para retornar tuplas
maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs m n p = (mx, eqCount)
  where mx = maxiThree m n p
...
-}
addEspacos :: Int -> String
addEspacos e | e > 0 = " " ++ addEspacos (e - 1) | otherwise = ""

addDireita :: Int -> String -> String
addDireita x y | x > 0 = addDireita (x-1) y  ++ " " 
	       | otherwise = y

boomBangs :: [Integer] -> [[Char]]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x]

funcX :: Float -> Float -> [Float]
funcX x y = [x*y^3 + 3*(sqrt x) | x > 0, y > 0]

tamanhoV :: [Integer] -> Integer
tamanhoV xs = sum [1 | _ <- xs ]
 
--tuplas
intP :: (Int, Int)
intP = (33,43)

addPair :: (Int, Int) -> Int
addPair (x,y) = y+x

--type Name = String
--type Age = Int
--type Phone = Int
--type Person = (Name, Age, Phone)

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

--fibo :: Int -> [Int]
--fibo n | n <= 1 = []
--       | otherwise = n : [(fibo(n-1)) + (fibo(n-2))]
--

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:b) x = (x==a) || member b x

digits :: String -> String
--digits (a:b) = [a | ( member [0..10] a)  (member) || b == []]
digits [] = []
digits (a:b) = if elem a ['0'..'9'] then a:digits b else digits b 

sumPairs :: [(Int,Int)] -> [Int]
sumPairs [] = []
sumPairs (x:y) = addPair x:sumPairs y

type Pessoa =  String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

banco :: BancoDados
banco = [("caralho","a historia da porra"), ("buceta","o caralho voador"), ("boquete","a merda por cima do predio")]

--member :: [Int] -> Int -> Bool
--member l i = if [x | x <- l, x == i] == [] then  = True 

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [l | (p1,l) <- bd, p1 == p ]

--emprestimos :: BancoDados -> Livro -> [Pessoa]
--emprestimos = bd l = [p | (p,l1) <- bd, l1 == l ]

--devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
--devolder bd pess livr = [(pessoa, livro) | (pessoa,livro) < bd, (pessoa,livro) /= (pess,livr)]




qsort :: [Int] -> [Int]
qsort [] = []
qsort (a:as) = qsort [ y | y <-as , y < a] ++ [a] ++ qsort [x | x <- as, x >= a ]

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
--insert x as = [y | y <- as, y < x] ++ [x] ++ [z | z <- as, z > x]
insert x (a:as) | x >= a = a:insert x as | otherwise = x:insert a as

iSort :: [Int] -> [Int]
iSort []=[]
iSort (a:as) = insert a (iSort as)  
