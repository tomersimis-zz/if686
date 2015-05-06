splitWords :: String -> [String]
splitWords str = splitWords' str ""

splitWords' :: String -> String -> [String]
splitWords' [] acc = [acc]
splitWords' (a:as) acc
	| a == ' ' && (length acc) > 0 = acc:(splitWords' as "")
	| a == ' ' && (length acc) == 0 = (splitWords' as "")
	| otherwise = splitWords' as (acc++[a])
	
dropSpace :: String -> String
dropSpace (a:as)
	| a == ' ' = dropSpace as
	| otherwise = a:as

getWord :: String -> String
getWord str = getWord' str ""

getWord' :: String -> String -> String
getWord' (a:as) acc
	| a == ' ' = acc
	| otherwise = getWord' as (acc++[a])

dropWord :: String -> String
dropWord (a:as)
	| a == ' ' = as
	| otherwise = dropWord as

foldr' :: (a->b->b) -> b -> [a] -> b
foldr' _ b [] = b
foldr' f b (a:as) = f a (foldr' f b as)
