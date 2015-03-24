-- MERGESORT

-- Complexity: O(n)
merge :: [Int] -> [Int] -> [Int]
merge left right
    | left  == []              = right
    | right == []              = left
    | head left <= head right  = (head left):(merge (tail left) right)
    | otherwise                = (head right):(merge left (tail right))

-- Complexity: O(n*n*log n)
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort ls = merge (mergeSort(fst (mySplitAt ((length ls) `div` 2) ls))) (mergeSort(snd (mySplitAt ((length ls) `div` 2) ls)))



-- HEAPSORT

-- Complexity: O(1)
left :: Int -> Int
left i = (2*i)

-- Complexity: O(1)
right :: Int -> Int
right i = (2*i + 1) 

-- Complexity: O(n)
swap :: Int -> Int -> [Int] -> [Int] -- 2 4 [1,5,7,9,3,2]
swap i j ls = (myTake (i-1) ls) ++ (ls!!(j-1):[]) ++ (myDrop (i) (myTake (j-1) ls) ) ++ (ls!!(i-1):[]) ++ (myDrop j (myTake (length ls) ls) )

-- Complexity: O(n*log n)
heapify :: Int -> [Int] -> [Int]
heapify i ls
	| ls == []      = []
	| right i <= length ls && ls!!((left i)-1) < ls!!((right i)-1) && ls!!((left i)-1) < ls!!(i-1) = (heapify (left i) (swap i (left i) ls))
	| right i <= length ls && ls!!((right i)-1) < ls!!((left i)-1) && ls!!((right i)-1) < ls!!(i-1) = (heapify (right i) (swap i (right i) ls))
	| left i <= length ls && ls!!((left i)-1) < ls!!(i-1) = (heapify (left i) (swap i (left i) ls))
	| otherwise = ls

-- Complexity: O(n*n*log n)
buildHeap :: Int-> [Int] -> [Int]
buildHeap i ls
	| i > 0 = buildHeap (i - 1) (heapify i ls) 
	| otherwise = ls

-- Complexity: O(n*log n)
goDown :: Int -> [Int] -> [Int]
goDown i ls
	| ls == []      = []
	| right i <= length ls && ls!!((left i)-1) < ls!!((right i)-1) && ls!!((left i)-1) < ls!!(i-1) = (goDown (left i) (swap i (left i) ls))
	| right i <= length ls && ls!!((right i)-1) < ls!!((left i)-1) && ls!!((right i)-1) < ls!!(i-1) = (goDown (right i) (swap i (right i) ls))
	| left i <= length ls && ls!!((left i)-1) < ls!!(i-1) = (goDown (left i) (swap i (left i) ls))
	| otherwise = ls

-- Complexity: O(n*n*log n*log n)
heapSort :: [Int] -> [Int]
heapSort ls
	| ls == [] = []
	| otherwise = head (buildHeap ((length ls) `div` 2) ls):heapSort (goDown 1 (tail (buildHeap ((length ls) `div` 2) ls)))

-- Complexity: O(n)
mySplitAt :: Int -> [Int] -> ([Int],[Int])
mySplitAt n ls = (myTake n ls, myDrop n ls)

-- Complexity: O(n)
myTake :: Int -> [Int] -> [Int]
myTake n ls = (myTake' 0 n ls)
myTake' :: Int -> Int -> [Int] -> [Int]
myTake' i n ls
	| i >= n = []
	| i < n = (head ls):(myTake' (i+1) n (tail ls))

-- Complexity: O(n)
myDrop :: Int -> [Int] -> [Int]
myDrop n ls = (myDrop' 0 n (length ls) ls)
myDrop' :: Int -> Int -> Int -> [Int] -> [Int]
myDrop' i n len ls
	| i >= n && i < len = (head ls):(myDrop' (i+1) n len (tail ls))
	| i < n = myDrop' (i+1) n len (tail ls)
	| otherwise = []
