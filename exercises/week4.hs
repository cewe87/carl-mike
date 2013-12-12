--import Test.QuickCheck

--data Expr = Lit Int
--          | Add Expr Expr
--          | Sub Expr Expr
--          | Mul Expr Expr
--          | Div Expr Expr

--eval :: Expr -> Int
--eval (Lit n)     = n
--eval (Add e1 e2) = eval e1 + eval e2
--eval (Sub e1 e2) = eval e1 - eval e2
--eval (Mul e1 e2) = eval e1 * eval e2
--eval (Div e1 e2) = eval e1 `div` eval e2

--showExpr :: Expr -> String
--showExpr (Lit n)     = show n
--showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
--showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++")"
--showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++")"
--showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++")"

----eval (Num 67)
----eval (Add (Sub (Lit 3) (Lit 1)) (Lit 3))
----showExpr (Add (Lit 67) (Lit -34))

--size :: Expr -> Int
--size (Add a b) = 1 + size a + size b
--size (Sub a b) = 1 + size a + size b
--size (Mul a b) = 1 + size a + size b
--size (Div a b) = 1 + size a + size b
--size _ = 0



--data NTree = NilT
--           | Node Int NTree NTree deriving (Show, Eq)

--rTree :: Int -> Gen NTree
--rTree s = frequency [(1,rNum),(s,rBin s)]  where 
--   rNum = elements [NilT]
--   rBin s = do 
--        let s' = (s `div` 2)
--        op <- elements [1,2,3,4,5,6,7,8,9]
--        e1 <- rTree s' 
--        e2 <- rTree s'
--        return (Node op e1 e2)

--instance Arbitrary NTree where
--  arbitrary = sized rTree

----We can sum the nodes and calculate the depths of a tree as follows
--sumTree :: NTree -> Int
--sumTree NilT           = 0
--sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

--depth :: NTree -> Int
--depth NilT           = 0
--depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

--leftSide :: NTree -> NTree
--leftSide (Node _ lTree _) = lTree

--rightSide :: NTree -> NTree
--rightSide (Node _ _ rTree) = rTree


--memberOfTree :: NTree -> Int -> Bool
--memberOfTree NilT _ = False 
--memberOfTree (Node n lTree rTree) needle = 
--	n  == needle || memberOfTree lTree needle || memberOfTree rTree needle

--reflect :: NTree -> NTree
--reflect NilT = NilT
--reflect (Node n lTree rTree) = (Node n (reflect rTree) ( reflect lTree))

--prop_reflect :: NTree -> Bool
--prop_reflect tree = (tree == reflect (reflect tree))

--deathTree = Node 2 (Node 2 (Node 2 NilT NilT) (Node 4 NilT (Node 5 NilT NilT))) (Node 7 NilT NilT) 

{-
A file either contains data or is a directory. 
A directory contains other files (which may themselves be directories) -
 along with a name for each one.

A. Design a data type to represent the contents of-
	 a directory. Ignore the contents of files: you are just
	  trying to represent file names and the way they are 
	  organised into directories here.
-}

data File = Data | Dir deriving (Show)
type Name  = String
data Dir = Empty | Files [(Name, File)]
type Path = String

 --B. Define a function to search for a given file name in a directory. You should return a path leading to a file with the given name. Thus if your directory contains a, b, and c, and b is a directory containing x and y, then searching for x should produce b/x.

findFile' :: Dir -> String -> Path
findFile' Empty fn = ""
findFile' (Files xs) fn = unlines [ if fn == name then name else "" |(name, f)<-xs]


isDir :: File -> Bool
isDir Data =  True
isDir _ = False




