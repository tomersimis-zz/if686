-- CODIGO ABAIXO = TRABALHO
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

-- CODIGO ABAIXO: EXERICICIOS DA AULA
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (menor a (menor b c), maior a (maior b c) )

menor :: Int -> Int -> Int
menor a b
	| a > b = b
	| otherwise = a

maior :: Int -> Int -> Int
maior a b
	| a > b = a
	| otherwise = b

interm :: Int -> Int -> Int -> Int
interm a b c
	| a <= b && b <= c = b
	| a <= c && c <= b = c
	| b <= a && a <= c = a
	| b <= c && c <= a = c
	| c <= a && a <= b = a
	| c <= b && b <= a = b

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c) = (menor a (menor b c), interm a b c, maior a (maior b c))

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

getX :: Ponto -> Float
getX (x,y) = x

getY :: Ponto -> Float
getY (x,y) = y

isVertical :: Reta -> Bool
isVertical (p1, p2) = getX p1 == getX p2

pontoY :: Float -> Reta -> Float
pontoY x ((x1,y1), (x2,y2)) = ((y2-y1)/(x2-x1))*x

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, String)]

baseExemplo :: BancoDados
baseExemplo =  [("Sergio","O Senhor dos Aneis"),("Andre","Duna"), ("Fernando","Jonathan Strange & Mr.Norrell"),  ("Fernando","A Game of Thrones")]

-- livros emprestados

--livros :: BancoDados -> Pessoa -> [Livro]
--livros [] p = []
--livros ((pessoa,livro):t) p
--	| pessoa == p = livro:livros t p
--	| otherwise = livros t p

--emprestimos :: BancoDados -> Livro -> [Pessoa]
--emprestimos [] l = []
--emprestimos ((pessoa,livro):t) l
--	| livro == l = pessoa:emprestimos t l
--	| otherwise = emprestimos t l

--emprestado :: BancoDados -> Livro -> Bool
--emprestado [] l = False
--emprestado ((pessoa,livro):t) l
--	| livro == l = True
--	| otherwise = emprestado t l


--qtdEmprestimos :: BancoDados -> Pessoa -> Int
--qtdEmprestimos [] p = 0
--qtdEmprestimos ((pessoa,livro):t) p
--	| pessoa == p = 1 + qtdEmprestimos t p
--	| otherwise = qtdEmprestimos t p

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd p l
	| not (emprestado bd l) = bd ++ [(p,l)]
	| otherwise = bd

--devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
--devolver [] p l = []
--devolver ((pessoa, livro):t) p l
--	| pessoa == p && livro == l = t
--	| otherwise = (pessoa, livro):(devolver t p l)

membro :: [Int] -> Int -> Bool
membro ls m = [m] == [x | x <- ls, x == m]


livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [ livro | (pessoa, livro) <- bd, pessoa==p]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [pessoa | (pessoa, livro) <- bd, livro==l]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l = [l] == [ livro | (pessoa, livro) <- bd, livro==l]

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length [ livro | (pessoa, livro) <- bd, pessoa==p]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd p l = [ (pessoa,livro) | (pessoa, livro) <- bd, not(pessoa == p) || not(livro == l) ]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (h:t) = quicksort([x | x <- t, x <= h]) ++ [h] ++ quicksort([x | x <- t, x > h])
