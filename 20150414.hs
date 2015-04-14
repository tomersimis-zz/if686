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
filterTree f tree = (filterNodes [(filterTree' f x) | x <- (getNodes f tree)]) ++ [filterTree' f tree]

getNodes :: (t -> Bool) -> Tree t -> [Tree t]
getNodes _ NilT = []
getNodes f (Node n left right)
	| f n = (getNodes f left) ++ (getNodes f right)
	| otherwise = [left] ++ [right] ++ (getNodes f left) ++ (getNodes f right)

filterNodes :: [Tree t] -> [Tree t]
filterNodes [] = []
filterNodes ((NilT):as) = filterNodes as
filterNodes ((Node n left right):as) = [(Node n left right)] ++ filterNodes as


-- AULA
filterNegSum :: Int -> [[Int]] -> [[Int]]
filterNegSum v ls = filter (\x -> (foldr (+) 0 x) > v) (ls)

isElement :: Int -> [Int] -> Bool
isElement x ls = length ([y | y <- ls, y == x]) > 0

inter :: [Int] -> [Int] -> [Int]
inter a b = filter (\x -> isElement x b) a

diff :: [Int] -> [Int] -> [Int]
diff a b = filter (\x -> not(isElement x b)) a

--mapFold :: (t -> u) -> (t -> t -> t) -> t -> [[t]] -> [t]
--mapFold fMap fFold iFold ls = [fMap x | x <- (foldr' fFold iFold ls)]

--foldr' :: (t -> t -> t) -> t -> [t] -> t
--foldr' _ i [] = i
--foldr' f i (a:as) = f a (foldr' f i as)

mapFilter :: (t -> Bool) -> [[t]] -> [[t]]
mapFilter f ls = [filter' f x | x <- ls]  

filter' :: (t -> Bool) -> [t] -> [t]
filter' _ [] = []
filter' f (a:as)
	| f a  = a:filter' f as
	| otherwise = filter' f as
