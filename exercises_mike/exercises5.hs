import Data.Maybe
import Test.QuickCheck

data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1   +   eval e2
eval (Sub e1 e2) = eval e1   -   eval e2
eval (Mul e1 e2) = eval e1   *   eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++")"

-- Exercise 0 B

example = (Add (Lit 1) (Add (Add (Lit 2) (Div (Lit 2) (Lit 0))) (Lit 3)))
example2 = Add (Lit 1) (Lit 2)

size :: Expr -> Int
size (Add a b) = 1 + size a + size b 
size (Sub a b) = 1 + size a + size b
size (Mul a b) = 1 + size a + size b 
size (Div a b) = 1 + size a + size b
size _         = 0

-- Exercise 0 C

-- TODO: Fix
eval' :: Expr -> Maybe Int
eval' e = undefined

-- Exercise 0 B

data NTree = NilT
           | Node Int NTree NTree
           deriving (Eq,Show)

treeOfDeath = (Node 2 ((Node 2 ((Node 2 ((Node 2 (NilT) (NilT))) ((Node 2 (NilT) ((Node 99 (NilT) (NilT))))))) (NilT))) ((Node 2 ((Node 2 (NilT) (NilT))) (NilT))))

sumTree :: NTree -> Int
sumTree NilT           = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth :: NTree -> Int
depth NilT           = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

left :: NTree -> NTree
left (Node _ l _) = l

right :: NTree -> NTree
right (Node _ _ r) = r

treeElem :: Int -> NTree -> Bool
treeElem _ NilT = False
treeElem i n    = i == e n || treeElem i (left n) || treeElem i (right n)
				  where e (Node ee _ _) = ee  

-- Exercise 0 E	

reflect :: NTree -> NTree
reflect NilT		 = NilT
reflect (Node n l r) = (Node n (reflect r) (reflect l)) 

prop_reflect :: NTree -> Bool
prop_reflect n = reflect (reflect n) == n

instance Arbitrary NTree where
	arbitrary = ntree

ntree :: Gen NTree
ntree = frequency [(3, return NilT),
					(2, do 
							x <- choose (1,100)
							t <- ntree
							t' <- ntree
							return (Node x t t'))
				  ]

-- Exercise 1 

data DTree = File String
	       | Directory String [DTree]
			deriving (Eq, Show)

--instance Arbitrary DTree where
--	arbitrary = dtree

--dtree :: Gen DTree
--dtree = frequency [
--					(3, return ( File "(arbitrary :: Gen String)" ) ),
--					(2, return ( Directory [(arbitrary :: dtree)] ) )
--				  ]

exampleDir = Directory "A" ([Directory "B" ([File "Magass"]), Directory "C" ([File "M"])])

search :: String -> DTree ->  String
search needle (File s) 		   = "/" ++ s
search needle (Directory x:xs) = search x 




