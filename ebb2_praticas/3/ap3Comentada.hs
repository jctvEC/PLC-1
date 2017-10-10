--Q1
data Pilha1 t = Null | Elem t (Pilha1 t) deriving Show
data Pilha2 t = Void | Casa t [Pilha2 t] deriving Show
--Os dados são os sugeridos pelo slide, não mudei nada


--Pilha1 = recursivo
push1 :: Pilha1 t -> Pilha1 t -> Pilha1 t --Push coloca um item na pilha
push1 (Null) (Elem a b) = (Elem a b)  --Se eu dou "push" de um elemento nulo em uma pilha, eu tenho a própria pilha
push1 (Elem a b) (Null) = (Elem a b) --Se eu dou push de uma pilha em uma pilha nula, eu retorno a própria pilha que eu "pushei"
push1 (Elem a b) (Elem c d) = (Elem a (push1 b (Elem c d))) --Se eu dou push de uma pilha a em uma pilha b, eu boto a pilha b no final da pilha a

pop1 :: Pilha1 t -> Pilha1 t --Pop retorna uma pilha nova sem um topo
pop1 (Null) = error "Pilha Vazia!" --Se a pilha tiver vazia, não tem o que tirar
pop1 (Elem a b) = b --Se a pilha tiver elemento, basta retornar a pilha sem o topo

top1 :: Pilha1 t -> t --top retorna o topo da pilha
top1 (Null) = error "Pilha Vazia!" --Se a pilha tiver vazia, não tem topo
top1 (Elem a b) = a --Caso contrário, basta pegar o primeiro item que já é nosso topo


--Pilha2 = lista
push2 :: Pilha2 t -> Pilha2 t -> Pilha2 t --A lógica é a mesma de pilha1, a única diferença é:
push2 (Void) (Casa a x) = (Casa a x)
push2 (Casa a x) (Void) = (Casa a x)
push2 (Casa a []) (Casa c x) = (Casa a [(Casa c x)]) --Agora nossa pilha é uma lista, então eu tenho que começar a concatenar os elementos em uma lista na ordem correta
push2 (Casa a x) (Casa c y) = (Casa a (x ++ [(Casa c y)])) --Caso eu tenho um elemento a na pilha x e quero dar um "push" nele na pilha y, que tem o topo c, eu tenho que colocar o topo c no fim da pilha x,
--por isso a (Casa c y) fica no final da lista

pop2 :: Pilha2 t -> Pilha2 t --Mesmo esquema de pilha1
pop2 (Void) = error "Pilha Vazia!"
pop2 (Casa a []) = (Void)
pop2 (Casa a ((Casa b x):bs)) = (Casa b (x++bs))  --A diferença é que eu tenho que retornar o primeiro elemento da lista como novo "topo", então eu tenho que concatenar a antiga lista com a lista do 
--primeiro elemento, por isso eu junto bs, que é o resto da lista antiga, com x, que é a lista do primeiro elemento

top2 :: Pilha2 t -> t --mesmo esquema de pilha1
top2 (Void) = error "Pilha Vazia!"
top2 (Casa a x) = a


--Q2
data LDisjointSet = LDS [[Int]] deriving (Eq, Show)
data TDisjointSet = Voidd | TDS [Int] TDisjointSet deriving (Eq, Show)

findl :: Int -> LDisjointSet -> Maybe Int
findl x (LDS []) = Nothing
findl x (LDS (a:as)) | (filter (== x) a /= []) = Just (head a)
                     | otherwise = findl x (LDS as)

findr :: Int -> TDisjointSet -> Maybe Int
findr x (Voidd) = Nothing
findr x (TDS a b) | (filter (==x) a) /= [] = Just (head a)
                  | otherwise = findr x b

makeSetl :: Int -> LDisjointSet -> LDisjointSet
makeSetl x (LDS b) | (findl x (LDS b)) == Nothing = (LDS (b ++ [[x]]))
                   | otherwise = (LDS b)

makeSetr :: Int -> TDisjointSet -> TDisjointSet
makeSetr x (Voidd) = (TDS [x] Voidd)
makeSetr x (TDS b c) | (findr x (TDS b c)) == Nothing = (TDS [x] (TDS (b) c))
                     | otherwise = (TDS b c)
{--
•A função makeSet :: Int -> t -> t, iria um novo ionjunto iujo úniio elemento é x,
desde que x não pertença a outro ionjunto da ioleção.
•A função union :: Int -> Int -> t -> t, exeiuta a união dos ionjuntos que iont m os
elementos x e y, transformando os dois ionjuntos em um úniio ionjunto.
•A função fnd :: Int -> t -> Maybe Int, retorna o representante do ionjunto que
iont m o elemento proiurado x. Caso x não pertença a nenhum dos ionjuntos,
Nothing deve ser retornado.
•A função makeSet nas duas instâniias deve ser implementada utlizando apenas a
função fnd.
--}


--Bonus
data Tree = Nulll | Leaf Int | Node Int (Tree) (Tree) deriving Show  --Árvore é ou folha (final da árvore), ou nó (tem um valor + subárvores esquerda, menores, e direita, maiores) ou n tem valor at all
 
insert1 :: Tree -> Tree -> Tree 
insert1 x (Nulll) = x --Se eu inserir uma árvore no nada, eu só vou ter uma árvore
insert1 (Nulll) x = x --Se eu inserir nada em uma árvore, eu só vou ter uma árvore
insert1 (Leaf a) (Leaf b) | a > b = (Node a (Leaf b) (Nulll)) --Se eu inserir uma folha em outra, eu preciso analisar pra onde vai o valor, se menor (esq) ou maior (dir)
                          | otherwise = (Node a (Nulll) (Leaf b))
insert1 (Node a l r) (Leaf b) | a > b = Node b (Nulll) (Node a l r) --Mesma coisa que 52, só que dessa vez inserindo um nó em uma folha
                              | otherwise =  Node b (Node a l r) (Nulll)
insert1 (Leaf a) (Node b c d) | a > b = (Node b c (insert1 (Leaf a) d)) --Inserindo uma folha em outro nó, eu preciso achar a posição da folha que tá sendo inserido, por isso eu chamo a função no lado que a folha vai na árvore
                              | otherwise = (Node b (insert1 (Leaf a) c) d)
insert1 (Node a b c) (Node x y z) | a > x = (Node x y (insert1 (Node a b c) z)) --Mesmo esquema da folha, só que dessa vez com dois nós
                                  | otherwise = (Node x (insert1 (Node a b c) y) z)

delete :: Int -> Tree -> Tree
delete x (Nulll) = (Nulll) --Não existe o número, seja qual for, no nada
delete x (Leaf y) | x == y = (Nulll) --Se for a folha que a gente tá procurando, então é só tirar ela e, embaixo dela não tem mais nenhum outro valor
                  | otherwise = (Leaf y) --Se não for, acabou esse ramo e o número não existe na árvore, então não dá pra tirar o que não existe
delete x (Node a l r) | x == a = (insert1 r l) --Se o número for o nó que a gente achou, a gente tem que "subir" a subárvore da esquerda. Pra isso, basta "inserir" a subárvore da esquerda na subárvore da direita
                      | x > a = (Node a l (delete x r)) --Se não for o número que a gente quer, basta continuar procurando. Pra isso, se o número for maior que o nó, tem que procurar na subárvore a direita e concatenar com a árvore que a gente já procurou
                      | otherwise = (Node a (delete x l) r) --Mesmo esquema de 65, só que dessa vez pra subárvore a esquerda

arvLista :: Tree -> [Int]  --Transforma uma árvore em uma lista
arvLista (Nulll) = [] --Se não tiver nada, não tem o que acrescentar a lista
arvLista (Leaf a) = [a] --Se for folha, só tem mais um único valor a ser acrescentado a lista
arvLista (Node a b c) = (arvLista b) ++ [a] ++ (arvLista c) --Caso contrário, eu tenho que adicionar o nó e as subárvores do nó

listArv :: [Int] -> Tree --Transformar uma lista em uma árvore
listArv [] = Nulll --Se não tiver nada, chegamos em um "nó nulo
listArv [x] = (Leaf x) --Se só tivermos um número, ele é uma folha e não tem mais ninguém depois dele
listArv (a:as) = insert1 (Leaf a) (listArv as) --Caso contrário, a gente precisa montar nossa árvore, usando a função insert que coloca uma folha com a cabeça de nossa lista na posição certa

somaArv :: Tree -> Int --Soma todos os números de uma árvore. Funciona do mesmo jeito que arvLista, só que, ao invés de concatenar, soma
somaArv (Nulll) = 0
somaArv (Leaf a) = a
somaArv (Node a b c) = (somaArv b) + a + (somaArv c)

maiorArv :: Tree -> Int --Altura da árvore
maiorArv (Nulll) = 0 --Se for um nó nulo, não conta como altura
maiorArv (Leaf a) = 1 --Se for a folha, é o último "andar" da árvore
maiorArv (Node a b c) = 1 + (max1 (maiorArv b) (maiorArv c)) --Eu preciso saber só da maior altura, então, se eu tiver em um nó, eu tenho o andar que eu cheguei (1 +) e a maior das alturas das subárvores

max1 :: Int -> Int -> Int
max1 a b | a > b = a
         | otherwise = b

