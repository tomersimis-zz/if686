data Failable t = Error String | Success t
	deriving (Show)

instance Monad Failable where
	(>>=) (Error x) _ = Error x
	(>>=) (Success x) f = f x
	return x = Success x


data Fila t = Node t Int (Fila t) | Nil 
	deriving(Show)


criarFila :: Int -> t -> Failable (t, Fila t)
criarFila size first
	| size < 1 = Error "Queue size too small"
	| otherwise = Success (first, Node first size (Nil))

push :: t -> Fila t -> Failable (t, Fila t)
push el (Node head size tail)
	| size > getSize (Node head size tail) = Success (el, (push' el (Node head size tail)))
	| otherwise = Error "Queue full"

push' :: t -> Fila t -> Fila t
push' el (Node head size Nil) = Node head size (Node el (size-1) Nil)
push' el (Node head size tail) = Node head size (push' el tail)

getSize :: Fila t -> Int
getSize Nil = 0
getSize (Node _ _ next) = 1 + getSize(next)

pop :: Fila t -> Failable (t, Fila t)
pop (Node head size tail)
	| (getSize (Node head size tail)) < 1 = Error "Queue is empty"
	| otherwise = Success $ pop' (Node head size tail)

pop' :: Fila t -> (t,Fila t)
pop' (Node head size Nil) = (head, Nil)
pop' (Node head size tail) = (removed, Node head size (snd newTail))
	where
		newTail = pop' tail
		removed = fst newTail

peek :: Fila t -> Failable(t, Fila t)
peek (Node head size tail)
	| (getSize (Node head size tail)) < 1 = Error "Queue is empty"
	| otherwise = Success (head, (Node head size tail))
