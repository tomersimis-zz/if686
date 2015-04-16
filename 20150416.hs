listPartitioner :: (Num a, Ord a) => [a] -> ([a] -> [[a]])
listPartitioner sep = listPartitioner' sep

listPartitioner' :: (Num a, Ord a) => [a] -> [a] -> [[a]]
listPartitioner' sep ls = listPartitioner'' (quicksort sep) ((smaller ls (head ls))-1) ls

listPartitioner'' :: (Num a, Ord a) => [a] -> a -> [a] -> [[a]]
listPartitioner'' [a] prev ls = [[x | x <- ls,  x > prev && x <= a]] ++ [(filter (>a) ls)]
listPartitioner'' (a:as) prev ls = [[x | x <- ls,  x > prev && x <= a]] ++ listPartitioner'' as a ls

smaller :: (Num a, Ord a) => [a] -> a -> a
smaller [] act = act
smaller (a:as) act
	| a < act = smaller as a
	| otherwise = smaller as act

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (a:as) = quicksort(filter (<a) as) ++ (a:(filter (==a) as)) ++ quicksort(filter (>a) as)


-- AULA

-- PARA FINS DE COMPILACAO
--f' :: (Num u, Num t) => u -> t -> v
--f' = (\a b -> f b a) 

getFirsts :: ([(t,t)] -> [t])
getFirsts = (\x -> [y | (y,z) <- x])

getBigger :: (Num t) => ([[t]] -> Int -> [[t]])
getBigger = (\x y -> [z | z <- x, (length z) > y])

remDupl :: (Eq t) => ([[t]] -> [t])
remDupl = (\x -> [y | y <- (foldr unite [] x) ]) 

unite :: (Eq t) => [t] -> [t] -> [t]
unite [] b = b
unite a [] = a
unite (a:as) b
	| a `elem` b = unite as b
	| otherwise = a:(unite as b)

sumAll :: (Num t) => t -> ([t] -> [t])
sumAll v = map (+v)

maxAll :: Ord t => ([t] -> t)
maxAll = (\x -> foldr (max) (minimum x) x)
