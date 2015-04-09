
import Data.Char (ord)
-- 1
-- Lista dos rotulos  +  Lista de arestas
data Graph t = Graph [t] [(t, t, Int)]
	deriving (Eq, Show)

-- 2
getNodes :: Eq t => Graph t -> t -> [t]
getNodes (Graph ls adj) f = [y | (x,y,z) <- adj, x == f]


search :: Eq t => Graph t -> t -> Bool
search (Graph ls adj) f = (length [x | x <- all_dfs, x == True]) > 0
	where
		graph = (Graph ls adj)
		all_dfs = [dfs graph x f | x <- ls]

dfs :: Eq t => Graph t -> t -> t -> Bool
dfs graph from to = dfs' graph [from] [] to

dfs' :: Eq t => Graph t -> [t] -> [t] -> t -> Bool -- graph, stack, visited, search
dfs' _ [] _ _ = False
dfs' (Graph ls adj) (a:as) vis f
	| a == f = True
	| isVisited vis a = dfs' graph as vis f
	| otherwise = dfs' graph new_stack new_visited f
		where
			graph = Graph ls adj
			new_stack = (getNodes graph a) ++ as
			new_visited = a:vis

isVisited :: Eq t => [t] -> t -> Bool
isVisited ls n = (length [x | x <- ls, x == n]) > 0

g :: Graph Char
g = Graph ['a', 'b', 'c', 'd'] [('a', 'b', 2), ('a', 'c', 3), ('c', 'd', 9), ('b', 'a', 0)]


-- AULA
getRoots :: Floating t => [t] -> [t]
getRoots ls = map sqrt ls

getPos :: Char -> Int
getPos c
	| c >= 'A' && c <= 'Z' = ord(c) - ord('A') + 1
	| c >= 'a' && c <= 'z' = ord(c) - ord('a') + 1
	| otherwise = -1

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto ls = map getPos ls

myMap :: (t -> u) -> [t] -> [u]
myMap f ls = [f x | x <- ls]

member :: Eq t => t -> [t] -> Bool
member f ls = foldr (||) False (map (==f) ls)


union :: [t] -> [t] -> [t]
union a b = foldr (:) [] a ++ foldr (:) [] b -- @_@

getSumOfChars :: [String] -> [Int]
getSumOfChars ls = map (foldr (+) 0) (map posicaoAlfabeto ls)

data Tree t = NilT | Node t (Tree t) (Tree t)
	deriving (Show)

t :: Tree Int
t = Node 7 (Node 4 NilT NilT) (Node 13 NilT NilT)

insert :: Ord t => Tree t -> t -> Tree t
insert (NilT) el = Node el NilT NilT
insert (Node a left right) el
	| el >= a = Node a left (insert right el)
	| otherwise = Node a (insert left el) right

{- DOESN'T WORK
criarArvore :: Ord t => [t] -> (Tree t -> t -> Tree t) -> Tree t
criarArvore ls f = foldr f NilT ls
-}
