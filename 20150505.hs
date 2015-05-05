import System.IO

-- Trabalho
check :: String -> Bool
check [] = True
check (a:as)
	| (a >= 'A' && a <= 'Z') || (a >= 'a' && a <= 'z') || a == ' ' = check as
	| otherwise = False

doCheck :: Maybe String -> Maybe String
doCheck (Just str)
	| check str = Just str
	| otherwise = Nothing

toUpperLetter :: Char -> Char
toUpperLetter c
	| c >= 'a' && c <= 'z' = toEnum ((fromEnum c)-32)
	| otherwise = c

toUpperCase :: Maybe String -> Maybe String
toUpperCase Nothing = Nothing
toUpperCase (Just str) = Just [toUpperLetter a | a <- str]

split :: Maybe String -> String -> [String]
split Nothing _ = [[]]
split (Just []) accum = [accum]
split (Just (a:as)) accum
	| a == ' ' = accum:(split (Just as) "")
	| otherwise = split (Just as) (accum++[a])

printList :: [String] -> IO ()
printList [] = return ()
printList (a:as) = do
	putStrLn a;
	printList as;

main :: IO ()
main = do
	putStrLn "Digite sua string:";
	str <- getLine;
	checked <- return (doCheck (Just str));
	upped <- return (toUpperCase checked);
	split <- return (split upped "");
	printList split;
	main;
