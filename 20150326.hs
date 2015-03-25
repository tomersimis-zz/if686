-- TRABALHO
type Hash a b = [(a,b)]

myHash :: Hash String Int
myHash = [("tomer", 5), ("joao", 7)]

hasKey :: Eq k => Hash k v -> k -> Bool
hasKey hash k = [k] == [ key | (key, value) <- hash, key == k ]

get :: Eq k => Hash k v -> k -> v
get hash k 
	| fst (head hash) == k = snd (head hash)
	| otherwise = get (tail hash) k

remove :: Eq k => Hash k v -> k -> Hash k v
remove hash k
	| fst (head hash) == k = tail hash
	| otherwise = (head hash):(remove (tail hash) k)

put :: Eq k => Hash k v -> k -> v -> Hash k v
put hash k v
	| (hasKey hash k) == True = (remove hash k) ++ [(k,v)]
	| otherwise = hash ++ [(k,v)]

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

