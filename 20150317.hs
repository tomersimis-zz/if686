qtd :: Int -> Int -> Int
qtd s n 
	| n < 0            = 0
	| (vendas n) == s  = 1 + (qtd s (n-1))
	| otherwise        = (qtd s (n-1))

	
vendas :: Int -> Int
vendas n = n
