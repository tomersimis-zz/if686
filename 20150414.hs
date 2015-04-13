compose :: (t -> u) -> [(v -> t)] -> [(v -> u)]
compose f ls = map (f . ) ls

data Graph t = Graph [t] [(t, t, Int)]
	deriving (Eq, Show)

g :: Graph Int
g = Graph [1, 2,3, 4] [(1, 2, 2), (1, 3, 3), (3, 4, 9), (2, 1, 0)]

mapGraph :: Graph t -> (t -> t) -> Graph t
mapGraph (Graph ls adj) f = (Graph ([f x | x <- ls]) new_adj)
	where
		new_adj = [(f x, f y,z) | (x,y,z) <- adj]

foldGraph :: Graph t -> (t-> t -> t) -> t -> t
foldGraph (Graph [] _) _ i = i
foldGraph (Graph (a:as) adj) f i = f a (foldGraph (Graph as adj) f i)

data Tree t = NilT | Node t (Tree t) (Tree t)
	deriving (Show,Eq)

t :: Tree Int
t = (Node 5 (Node 7 (Node 15 NilT (Node 6 NilT NilT)) (Node 2 NilT NilT)) (Node 10 NilT NilT))

filterTree' :: (t -> Bool) -> Tree t -> Tree t
filterTree' _ NilT = NilT
filterTree' f (Node n left right)
	| f n = (Node n (filterTree' f left) (filterTree' f right))
	| otherwise = NilT

filterTree :: (t -> Bool) -> Tree t -> [Tree t]
filterTree _ NilT = []
filterTree f (Node n left right)
	| f n = [(Node n  (filterTree' f left) (filterTree' f right))]
	| otherwise = [(filterTree' f left)] ++ [(filterTree' f right)]
