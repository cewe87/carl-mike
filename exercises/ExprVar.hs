module ExprVar where

import Test.QuickCheck
import Test.QuickCheck.Gen -- use quickCheck generators in other programs
import System.Random
import Data.List

import Data.Maybe(fromJust)
------------------------------------------------------------------

data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Var Name
 deriving ( Eq )

type Name = String

vars :: Expr -> [Name]
vars (Var v)   = [v] 
vars (Add e f) = vars e `union` vars   f
vars (Mul e f) = vars e `union` vars   f
vars _         = []

ex1 = Mul (Add (Num 1) (Num 2)) (Num 4) 
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))
ex3 = Num 1 `Add` (Num 2 `Mul` Num 4)

eval :: [(Name,Integer)] -> Expr -> Integer
eval _   (Num m)   = m
eval env (Add e f) = eval env e + eval env f
eval env (Mul e f) = eval env e * eval env f
eval env (Var x)   = fromJust (lookup x env)

-----------------------------------------------------------------------
instance Show Expr where
 show = showExpr

showExpr :: Expr -> String
showExpr (Var x)   = x
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e


-----------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized arbExpr

--
arbExpr :: Int -> Gen Expr
arbExpr s =
  frequency [ (1,genNum), (1, genVar)
            , (s,genOp Add) 
            , (s,genOp Mul)
        ]
  where genNum = do n <- arbitrary
                    return (Num n)
        genOp op = do a <- arbExpr s'
                      b <- arbExpr s'
                      return (op a b)
        s' = s `div` 2

genVar = elements [Var "x", Var "y", Var "z"]




----------------------------------------------------------------