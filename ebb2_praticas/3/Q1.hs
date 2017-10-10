--Q1
data Pilha1 t = Null | Elem t (Pilha1 t) deriving Show
data Pilha2 t = Void | Casa t [Pilha2 t] deriving Show

push1 :: Pilha1 t -> Pilha1 t -> Pilha1 t
push1 (Null) (Elem a b) = (Elem a b)
push1 (Elem a b) (Null) = (Elem a b)
push1 (Elem a b) (Elem c d) = (Elem a (push1 b (Elem c d)))

pop1 :: Pilha1 t -> Pilha1 t
pop1 (Null) = error "Pilha Vazia!"
pop1 (Elem a b) = b

top1 :: Pilha1 t -> t
top1 (Null) = error "Pilha Vazia!"
top1 (Elem a b) = a

push2 :: Pilha2 t -> Pilha2 t -> Pilha2 t
push2 (Void) (Void) = (Void)
push2 (Void) (Casa a x) = (Casa a x)
push2 (Casa a x) (Void) = (Casa a x)
push2 (Casa a []) (Casa c x) = (Casa a [(Casa c x)])
push2 (Casa a x) (Casa c y) = (Casa a (x ++ [(Casa c y)]))

pop2 :: Pilha2 t -> Pilha2 t
pop2 (Void) = error "Pilha Vazia!"
pop2 (Casa a []) = (Void)
pop2 (Casa a ((Casa b x):bs)) = (Casa b (x++bs))

top2 :: Pilha2 t -> t
top2 (Void) = error "Pilha Vazia!"
top2 (Casa a x) = a