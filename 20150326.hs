-- TRABALHO
type Hash = [(Int,Int)]

myHash :: Hash
myHash = [(0, 0), (1, 2) , (2,4) , (3,6), (4,8)]

hasKey :: Hash -> Int -> Bool
hasKey hash k = k == fst(hash!!(mod k (length hash)))

get :: Hash -> Int -> Int
get hash k 
	| hasKey hash k = snd(hash!!(mod k (length hash)))

remove :: Hash -> Int -> Hash
remove hash k = (take (mod k (length hash)) hash) ++ [(-1,-1)] ++ drop ((mod k (length hash)) +1 ) hash

put :: Hash -> Int -> Int -> Hash
put hash k v = (take (mod k (length hash)) hash) ++ [(k,v)] ++ drop ((mod k (length hash)) +1 ) hash

removeRepetido :: Eq t => [t] -> [t]
removeRepetido [] = []
removeRepetido (a:as) = a:(removeRepetido [x | x <- as, not (x == a)])

qtdElem :: Eq t => [t] -> [t] -> Int
qtdElem [] _ = 0
qtdElem _ [] = 0
qtdElem (a:as) b = (length [x | x <- b, x == a]) + (qtdElem as b)

comparaConjuntos :: Eq t => [t] -> [t] -> String
comparaConjuntos a b
	| aInB == 0 = "Conjuntos disjuntos"
	| aInB == bInA && sa' == sb' = "A igual a B"
	| aInB == sa' = "B contem A"
	| bInA == sb' = "A contem B"
	| otherwise = "A interseciona B"
	where 
		a' = removeRepetido a
		b' = removeRepetido b
		sa' = length a'
		sb' = length b'
		aInB = qtdElem a' b'
		bInA = qtdElem b' a'

