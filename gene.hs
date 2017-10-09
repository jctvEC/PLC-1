total :: (Int->Int) -> Int -> Int
total f 0 = f 0
total f n = total f (n - 1) + f n


sumSquares :: Int -> Int
sumSquares n = total sq n where
				sq x = x*x
funcT :: Int -> Int
funcT 0 = -1
funcT 1 = 0
funcT 2 = 1
funcT 3 = 2

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n | f n >= f (n - 1) = isCrescent f (n - 1)
	       | otherwise = False

maping :: (t -> u) -> [t] -> [u]
maping f [] = []
maping f (a:as) = f a : maping f as

sqrList xs = map sq xs where sq x = x*x

snds :: [(t,u)] -> [u]
snds xs = maping snd xs

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

foldr2 :: (t -> t -> t) -> [t] -> t
foldr2 f [a] = a
foldr2 f (a:as) = f a (foldr1 f as)

filter1 :: (t -> Bool) -> [t] -> [t]
filter1 p [] = []
filter1 p (a:as)
	| p a = a : filter1 p as
	| otherwise = filter1 p as

evens xs = filter isEven xs
	where isEven n = (n `mod` 2 == 0)

filter1' p l = [a | a <- l , p a]

maping' f l = [f a | a <- l]


