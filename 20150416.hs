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
