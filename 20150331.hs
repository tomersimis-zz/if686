{- 
1ª) 
O polimorfismo de haskell é mais seguro quanto a garantia de
que os tipos terão implementações das funcionalidades que serão 
utilizadas sob eles, pois é possível restringir que os argumentos genéricos 
possuam implementações de determinadas funções, sendo esses definidos por uma
classe. Em Java, o Generics que é utilizado no polimorfismo acaba sendo mais 
geral pois não restringe os dados, porém é mais sucetível à erro tendo em vista que
apenas ocorre uma verificação de tipo em tempo de execução e depois ocorre um "type erasure"
para gerar compatibilidade com as JVM antigas, pois Generics foi adicionada a partir do Java 5.
-}

-- 2a
count :: (Eq t, Num t) => t -> [t] -> t
count _ [] = 0
count n (a:as)
	| a == n = 1 + count n as
	| otherwise = 0

remove :: Eq t => t -> [t] -> [t]
remove _ [] = []
remove n (a:as)
	| a == n = remove n as
	| otherwise = a:as

countAndRemove :: (Eq t, Num t) => [t] -> [t]
countAndRemove [] = []
countAndRemove (a:as) = [(count a as) + 1] ++ [a] ++ countAndRemove (remove a as)

lookAndSay :: (Eq t, Num t) => Int -> t -> [t]
lookAndSay 0 _ = []
lookAndSay n i = lookAndSay' n [i]

lookAndSay' :: (Eq t, Num t) => Int -> [t] -> [t]
lookAndSay' _ [] = []
lookAndSay' n as
	| n > 1 = lookAndSay' (n-1) (countAndRemove as)
	| otherwise = as


-- 3a
type Node n = (n, [n])
type Graph n = [Node n]

g :: Graph Int
g = [(1, [2,3,4,6]), (2, [1]), (3, [1]), (4, [1,6,5]), (5, [4,6]), (6, [1,4,5]), (7,[8]), (8,[7])]

isVisited :: Eq t => t -> [t] -> Bool
isVisited _ [] = False
isVisited n (a:as)
	| n == a = True
	| otherwise = isVisited n as

getNodes :: Eq t => Graph t -> t -> [t]
getNodes [] _ = []
getNodes ((n,ns):as) i
	| n == i = ns
	| otherwise = getNodes as i

search :: Eq t => Graph t -> t -> t -> [(t,t)]
search [] _ _ = []
search g i j = getPath j (tail (reverse (reverse (dfs g [] [(i,i)]) )))

-- Graph, Visited nodes, stack
dfs :: Eq t => Graph t -> [t] -> [(t,t)] -> [(t,t)]
dfs _ _ [] = []
dfs g vis ((parent, node):as)
	| not (isVisited node vis) = (parent,node):dfs g (node:vis) ([(node, child) | child <- (getNodes g node) ] ++ as)
	| otherwise = dfs g vis as

getPath :: Eq t => t -> [(t,t)] -> [(t,t)]
getPath _ [] = []
getPath i path
	| ls == [] = []
	| otherwise = (getPath (fst (head ls)) ls') ++ ls
	where
		ls = [x | x <- path, (snd x) == i]
		ls'  = [x | x <- path, not((snd x) == i)]
		
		
quickSort :: (Ord x) => [x] -> [x]
quickSort [] = []
quickSort (h:t) =  quickSort([ a|a <- t, a < h  ])++(h:[])++quickSort([ a|a <- t, a >= h  ])

mediana :: [Int] -> Int
mediana [] = 0
mediana mylist
	| (mod (length lista) 2) == 1 = lista!!( (div (length lista) 2)  ) 
	| otherwise = div ((lista!!(div (length lista) 2 ))+(lista!!( (div (length lista) 2)-1  ) )) 2
	where lista = quickSort mylist

iterOver :: [x] -> Int -> Int -> [x]
iterOver [] a b = []
iterOver lista a b
	| a > b || b<= 0 ||  ( a > (length lista )) = []
	| a <= 0 = (iterOver lista (a+1) b)	
	| otherwise = ([lista!!(a-1)])++(iterOver lista (a+1) b)

getAll :: [[Int]] ->  [Int]
getAll [] = []
getAll (h:t) = h++(getAll t)

getMediana :: [[Int]] -> Int -> Int -> Int -> Int
getMediana matrix i j n = mediana all
	where rows = (iterOver matrix (i-(n-1)) (i+(n-1))); cols = [ (iterOver a (j-(n-1)) (j+(n-1))) |a <- rows  ]; all = getAll cols


pvt_filtroMediana :: [[Int]] -> Int -> Int -> Int -> [Int] -> [[Int]]
pvt_filtroMediana [] i j n col = []
pvt_filtroMediana matrix i j n col
	| j > (length (matrix!!0)) = ([col])++(pvt_filtroMediana matrix (i+1) 1 n [] ) 
	| i > (length matrix ) = []
	| otherwise = pvt_filtroMediana matrix i (j+1) n colAux
	where med = (getMediana matrix i j n); colAux = col++[med]

filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana matrix n = pvt_filtroMediana matrix 1 1 n []

baseMatrix :: [[Int]] 
baseMatrix = [[1,2,3],[4,5,6], [7,8,9]]

-- EXERCICIO
afd :: String -> [Int] -> [(Int, Int, Char)] -> Int -> [Int] -> Bool
afd [] _ _ _ _ = False
afd input states trans initial acc = simulate states trans acc [(input,initial)] 

simulate :: [Int] -> [(Int, Int, Char)] -> [Int] -> [(String,Int)] -> Bool
simulate _ _ _ [] = False
simulate states trans acc ((input, actual):as)
	| ((input, actual):as) == [] = False
	| [actual] == [x | x <- acc, x == actual] = True
	| otherwise = simulate states trans acc ([(tail input, y) | (x,y,z) <- trans, x == actual && z == head input] ++ as) 

--

tab :: [(Char, Int)]
tab = [('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7), ('8', 8), ('9', 9), ('A', 10), ('B', 11), ('C', 12), ('D', 13), ('E', 14), ('F', 15)]

hexToDec :: Char -> [(Char, Int)] -> Int
hexToDec n tab = head [dec | (hex,dec) <- tab, n == hex]

decToHex :: Int -> [(Char, Int)] -> Char 
decToHex n tab = head [hex | (hex,dec) <- tab, n == dec]

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

hexToDecList :: [Char] -> [(Char, Int)] -> [Int]
hexToDecList ls tab = [hexToDec x tab | x <- ls]

somatorioHexadecimal :: [Char] -> [(Char, Int)] -> [Char]
somatorioHexadecimal ls tab = reverse (somatorioHexadecimal' (sumList (hexToDecList ls tab)) tab)

somatorioHexadecimal' :: Int -> [(Char,Int)] -> [Char]
somatorioHexadecimal' n tab
	| n >= 16 = (decToHex m tab):somatorioHexadecimal' d tab
	| otherwise = [decToHex n tab]
		where
			d = n `div` 16
			m = n `mod` 16


