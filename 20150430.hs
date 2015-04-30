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

getSize :: Fila t -> Int
getSize Nil = 0
getSize (Node _ _ next) = 1 + getSize(next)
