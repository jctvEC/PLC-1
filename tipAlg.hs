data Shape = Circle Float | Rectangle Float Float deriving(Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float 
area (Circle r) = pi*(r^2)
area (Rectangle a b) = a*b

data Expr = Lit Int
	   | Add Expr Expr
	   | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)

data List t = Nil | Cons t (List t) deriving(Show)

data Tree t = NilT | Node t (Tree t) (Tree t) deriving(Show)

toList :: List t -> [t]
toList (Nil) = [] 
toList (Cons a b) = a:(toList b)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = (Cons a (fromList as))

