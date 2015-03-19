import Data.Char

double :: [Int] -> [Int]
double ls
	| ls == [] = []
	| otherwise = (head ls)*2:(double (tail ls))
	
membership :: [Int] -> Int -> Bool
membership ls m
	| ls == []       = False
	| (head ls) == m = True
	| otherwise      = membership (tail ls) m
	
digits :: String -> String
digits s
	| s == []                  = []
	| isDigit (head s) == True = (head s):(digits (tail s))
	| otherwise                = digits(tail s)
	
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs ls1 ls2
	| ls1 == [] && ls2 == [] = []
	| ls1 == [] = (head ls2):(sumPairs ls1 (tail ls2))
	| ls2 == [] = (head ls1):(sumPairs (tail ls1) ls2)
	| otherwise = ((head ls1)+(head ls2)):(sumPairs (tail ls1) (tail ls2))

smaller :: [Int] -> Int -> [Int]
smaller ls n
	| ls == [] = []
	| (head ls) < n = (head ls):(smaller (tail ls) n)
    | otherwise = smaller (tail ls) n

greater :: [Int] -> Int -> [Int]
greater ls n
	| ls == [] = []
	| (head ls) >= n = (head ls):(greater (tail ls) n)
    | otherwise = greater (tail ls) n

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (h:t) = (quicksort (smaller t h)) ++ [h] ++ (quicksort (greater t h))
	