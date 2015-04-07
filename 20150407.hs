data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle n) = 3.14*n*n
area (Rectangle a b) = a*b

data Dia = Segunda Int [String] | Terca Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado | Domingo

isWeekend :: Dia -> Bool
isWeekend Sabado = True
isWeekend Domingo = True
isWeekend _ = False

hasPLC :: Dia -> Bool
hasPLC Sabado = False
hasPLC Domingo = False
hasPLC (Segunda a b) = ["PLC"] == [x | x <- b, x == "PLC"]
hasPLC (Terca a b) = ["PLC"] == [x | x <- b, x == "PLC"]
hasPLC (Quarta a b) = ["PLC"] == [x | x <- b, x == "PLC"]
hasPLC (Quinta a b) = ["PLC"] == [x | x <- b, x == "PLC"]
hasPLC (Sexta a b) = ["PLC"] == [x | x <- b, x == "PLC"]

data Tree t = NilT | Node t (Tree t) (Tree t)
	deriving (Eq, Show)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
	deriving (Show)

showExpr :: Expr -> String
showExpr (Lit a) = show(a)
showExpr (Add a b) = showExpr(a) ++ "+" ++ showExpr(b)
showExpr (Sub a b) = showExpr(a) ++ "-" ++ showExpr(b)

data List t = Nil | Cons t (List t)
	deriving (Show)

toList :: List t -> [t]
toList (Nil) = []
toList (Cons a ls) = a:toList(ls)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)

depth :: Tree t -> Int
depth (NilT) = 0
depth (Node a b c) = 1 + (max (depth b) (depth c))

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node a b c) = [a] ++ collapse b ++ collapse c

-- DFS ftw
dfs :: Eq t => Tree t -> t -> Bool
dfs NilT _ = False
dfs (Node a b c) f
	| a == f = True
	| otherwise = (dfs b f) || (dfs c f)


{-
bfs :: Eq t => Tree t -> t  -> Bool
bfs NilT _ = False
bfs (Node a b c) f = bfs' (Node a b c) f []

bfs' :: Eq t => Tree t -> t -> [t] -> Bool
bfs' _ _ [] = False
bfs' (Node a b c) f (h:t)
	| h == f = True
	| otherwise = (bfs' b f )
-}

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node a b c) = Node (f a) (mapTree f b) (mapTree f c)
