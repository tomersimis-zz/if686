qtd :: Int -> Int -> Int
qtd s n 
	| n < 0            = 0
	| (vendas n) == s  = 1 + (qtd s (n-1))
	| otherwise        = (qtd s (n-1))

	
vendas :: Int -> Int
vendas n = n


quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (a:as) = quicksort ([x | x <- as, x < a]) ++ (a:[x | x <- as, x == a]) ++ quicksort([x | x <- as, x > a])

fibNeven :: Int -> [Int]
fibNeven 0 = []
fibNeven x = fibNeven' x 0

fibNeven' :: Int -> Int -> [Int]
fibNeven' 0 _ = []
fibNeven' n a
	| (f `mod` 2) == 0 = f:(fibNeven' (n-1) (a+1))
	| otherwise = fibNeven' n (a+1)
	where f = fib a

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = (fib $ x-1) + (fib $ x-2)

orden :: [Int] -> [Int]
orden [] = []
orden (a:as) = (orden [x | x <- as, (sumd x) < (sumd a)]) ++ (a:[x | x <- as, (sumd x) == (sumd a)]) ++ (orden [x | x <- as, (sumd x) > (sumd a)])

sumd :: Int -> Int
sumd 0 = 0
sumd x = (x `mod` 10) + (sumd (x `div` 10))
