{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map as M
import Debug.Trace
-- import ExprT as 
import Parser
import qualified StackVM as SVM

--ex 1
-- eval :: ExprT -> Integer
-- eval (Lit x) = x
-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Mul e1 e2) = eval e1 * eval e2

-- --ex 2
-- evalStr :: String -> Maybe Integer
-- evalStr = (fmap eval) . parseExp Lit Add Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

-- instance Expr ExprT where
--     lit = Lit
--     add = Add
--     mul = Mul

-- reify :: ExprT -> ExprT
-- reify = id

-- ex 4
-- Integer Expr
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (0 <)
    add = (&&)
    mul = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y)= MinMax (max x y)
    mul (MinMax x) (MinMax y)= MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y)= Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y)= Mod7 ((x * y) `mod` 7)

----------------
--ex5 StackVM
instance Expr SVM.Program where
    lit x = [SVM.PushI x]
    add x y = x ++ y ++ [SVM.Add]
    mul x y = x ++ y ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile s = parseExp lit add mul s

-- ex6 HasVars
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
   deriving (Eq, Show)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul
instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var key = trace "Var Called" (M.lookup key)

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = trace "lit Called" (\_ -> Just x)
    add x y = trace "Add Called" (\varMap -> pure (trace "Adding Values" (+)) <*> x varMap <*> y varMap)
    mul x y = trace "Mul Called" (\varMap -> pure (trace "Multiplying Values" (*)) <*> x varMap <*> y varMap)

withVars :: [(String, Integer)]
        -> (M.Map String Integer -> Maybe Integer)
        -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

