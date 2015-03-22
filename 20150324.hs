-- MERGESORT
merge :: [Int] -> [Int] -> [Int]
merge left right
    | left  == []              = right
    | right == []              = left
    | head left <= head right  = (head left):(merge (tail left) right)
    | otherwise                = (head right):(merge left (tail right))

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort ls = merge (mergesort(fst (splitAt ((length ls) `div` 2) ls))) (mergesort(snd (splitAt ((length ls) `div` 2) ls)))


-- QUICKSORT
left :: Int -> Int
left i = (2*i)

right :: Int -> Int
right i = (2*i + 1) 

swap :: Int -> Int -> [Int] -> [Int] -- 2 4 [1,5,7,9,3,2]
swap i j ls = (take (i-1) ls) ++ (ls!!(j-1):[]) ++ (drop (i) (take (j-1) ls) ) ++ (ls!!(i-1):[]) ++ (drop j (take (length ls) ls) )

heapify :: Int -> [Int] -> [Int]
heapify i ls
	| ls == []      = []
	| right i <= length ls && ls!!((left i)-1) < ls!!((right i)-1) && ls!!((left i)-1) < ls!!(i-1) = (heapify (left i) (swap i (left i) ls))
	| right i <= length ls && ls!!((right i)-1) < ls!!((left i)-1) && ls!!((right i)-1) < ls!!(i-1) = (heapify (right i) (swap i (right i) ls))
	| left i <= length ls && ls!!((left i)-1) < ls!!(i-1) = (heapify (left i) (swap i (left i) ls))
	| otherwise = ls

buildHeap :: Int-> [Int] -> [Int]
buildHeap i ls
	| i > 0 = buildHeap (i - 1) (heapify i ls) 
	| otherwise = ls

goDown :: Int -> [Int] -> [Int]
goDown i ls
	| ls == []      = []
	| right i <= length ls && ls!!((left i)-1) < ls!!((right i)-1) && ls!!((left i)-1) < ls!!(i-1) = (goDown (left i) (swap i (left i) ls))
	| right i <= length ls && ls!!((right i)-1) < ls!!((left i)-1) && ls!!((right i)-1) < ls!!(i-1) = (goDown (right i) (swap i (right i) ls))
	| left i <= length ls && ls!!((left i)-1) < ls!!(i-1) = (goDown (left i) (swap i (left i) ls))
	| otherwise = ls


heapsort :: [Int] -> [Int]
heapsort ls
	| ls == [] = []
	| otherwise = head (buildHeap ((length ls) `div` 2) ls):heapsort (goDown 1 (tail (buildHeap ((length ls) `div` 2) ls)))

