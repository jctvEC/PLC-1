--Questao 1
logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

contAcess :: String -> Int -> Int -> (Int,Int)
contAcess [] a b = (a,b)
contAcess (x:xs) a b | x=='N' = contAcess xs (a+1) b
		     | x=='D' = contAcess xs a (b+1)
		     | otherwise = contAcess xs a b

tiposDeAcesso :: String -> (Int,Int)
tiposDeAcesso xs = contAcess xs 0 0

--Questao 2

{-checkStr :: String -> String
checkStr [] = []
checkStr (x:xs) | x /= ';' = [x] ++ checkStr xs 
		| otherwise = []

listOfStr :: String -> [String]
listOfStr [] = []
listOfStr (a:as) | a == ';' = listOfStr as
 		 | otherwise = [checkStr as] ++ listOfStr as
-}

strToInt :: String -> Int
strToInt str = read str

checkJump :: String -> String
checkJump [] = []
checkJump (x:xs) | x /= '\n' = checkJump xs
		 | otherwise = xs

checkDay :: String -> Int -> String
checkDay [] _ = []
checkDay (x:xs) cont | x == '-' = checkDay xs (cont + 1)
		     | cont == 2 = (x:xs)
		     | otherwise = checkDay xs cont

retDay :: String -> String
retDay [] = []
retDay (a:as) = [a] ++ [(head as)]

listOfDays :: String -> [Int]
listOfDays [] = []
listOfDays str = [strToInt (retDay (checkDay str 0))] ++ listOfDays (checkJump str)

position :: Int -> [Int] -> [Int]
position x l = [b | (a,b) <- zip l [0..(length l)], a == x]

noRep :: [Int] -> [Int]
noRep [] = []
noRep (x:xs) | elem x xs = noRep xs
	     | otherwise = x:(noRep xs)

acessoPorDia :: String -> [(Int,Int)]
acessoPorDia str = tupCount (listOfDays str) (noRep (listOfDays str))

tupCount :: [Int] -> [Int] -> [(Int,Int)]
tupCount _ [] = []
tupCount l (x:xs) = [(x,length (position x l))] ++ tupCount l xs

--Questao 3

checkUser :: String -> Int -> String
checkUser [] _ = []
checkUser (x:xs) cont | x == ';' = checkUser xs (cont + 1)
		     | cont == 3 = (x:xs)
		     | otherwise = checkUser xs cont

retUser :: String -> String
retUser [] = []
retUser (a:as) | a /= ';' = a:(retUser as)
	       | otherwise = []

listOfUsers :: String -> [Int]
listOfUsers [] = []
listOfUsers str = [strToInt (retUser (checkUser str 0))] ++ listOfUsers (checkJump str)

acessoPorUsuario :: String -> [(Int,Int)]
acessoPorUsuario str = tupCount (listOfUsers str) (noRep (listOfUsers str))

--Questao 4

type Dia = String
type Hora = String
type Usuario = String

data LogEntry = Permitido Dia Hora Usuario
              | Negado Dia Hora Usuario

     deriving Show

checkHora :: String -> Int -> String
checkHora [] _ = []
checkHora (x:xs) cont | x == ';' = checkHora xs (cont + 1)
		     | cont == 1 = (x:xs)
		     | otherwise = checkHora xs cont

retHora :: String -> String
retHora [] = []
retHora (a:as) | a /= ';' = a:(retUser as)
	       | otherwise = []

listHoras :: String -> [Int]
listHoras [] = []
listHoras str = [strToInt (retHora (checkHora str 0))] ++ listOfHora (checkJump str)

completeConv :: String -> [Int] -> [Int] -> [Int] -> [LogEntry]
completeConv [] _ _

converte :: String -> [LogEntry]
converte str = completeConv str (listOfDays str) (listHoras str) (listOfUsers str)


