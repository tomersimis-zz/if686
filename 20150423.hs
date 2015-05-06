splitWords :: String -> [String]
splitWords str = splitWords' str ""

splitWords' :: String -> String -> [String]
splitWords' [] acc = [acc]
splitWords' (a:as) acc
	| a == ' ' && (length acc) > 0 = acc:(splitWords' as "")
	| a == ' ' && (length acc) == 0 = (splitWords' as "")
	| otherwise = splitWords' as (acc++[a])
	
