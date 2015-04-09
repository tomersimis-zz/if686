-- 1
-- Lista dos rotulos  +  Lista de arestas
data Graph t = Graph [t] [(t, t, Int)]
	deriving (Eq, Show)

-- 2
getNodes :: Eq t => Graph t -> t -> [t]
getNodes (Graph ls adj) f = [y | (x,y,z) <- adj, x == f]

dfs :: Eq t => Graph t -> t -> t -> Bool
dfs graph from to = dfs' graph [from] [from] to

dfs' :: Eq t => Graph t -> [t] -> [t] -> t -> Bool -- graph, stack, visited, search
dfs' _ [] _ _ = False
dfs' (Graph ls adj) (a:as) vis f
	| a == f = True
	| otherwise = dfs' (Graph ls adj) new_stack new_visited f
		where
			graph = Graph ls adj
			new_stack = (getNodes graph a) ++ as
			new_visited = a:vis

isVisited :: Eq t => [t] -> t -> Bool
isVisited ls n = (length [x | x <- ls, x == n]) > 0

g :: Graph Char
g = Graph ['a', 'b', 'c', 'd'] [('a', 'b', 2), ('a', 'c', 3), ('c', 'd', 9)]
