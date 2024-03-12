module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars e = free_vars' e [] []
    where
        free_vars' :: Expr -> [String] -> [String] -> [String]
        free_vars' (Variable x) current acc = if x `elem` acc then acc else if x `elem` current then acc else x : acc
        free_vars' (Function x e) current acc = free_vars' e (x : current) acc
        free_vars' (Application e1 e2) current acc = free_vars' e2 current $ free_vars' e1 current acc
-- TODO 1.2. reduce a redex

stringsOfLength :: Int -> [String]
stringsOfLength 0 = [""]
stringsOfLength n = concatMap (\c -> map (c :) $ stringsOfLength $ n - 1) ['a'..'z']


infiniteStrings :: [String]
infiniteStrings = concatMap stringsOfLength [1..]

findFirstNotIn :: [String] -> [String] -> String
findFirstNotIn (x : xs) ys = if x `elem` ys then findFirstNotIn xs ys else x

newVar :: Expr -> Expr -> String
newVar e1 e2 = findFirstNotIn infiniteStrings $ free_vars e1 ++ free_vars e2


reduce :: Expr -> String -> Expr -> Expr
reduce (Variable y) x e1 = if x == y then e1 else Variable y
reduce (Application e e') x e1 =  Application (reduce e x e1) (reduce e' x e1)
reduce (Function y e) x e1 =
    if x == y 
        then Function y e
    else if y `elem` (free_vars e1)
        then reduce (Function (newVar e e1) (reduce e y $ Variable $ newVar e e1)) x e1
    else Function y $ reduce e x e1

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Macro x) = Macro x
stepN (Variable x) = Variable x
stepN (Function x e) = Function x $ stepN e
stepN (Application (Function x e) e1) = reduce e x e1
stepN (Application (Variable x) e) = Application (Variable x) $ stepN e
stepN (Application e1 e2) = Application (stepN e1) e2

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN e = if e == stepN e then e else reduceN $ stepN e

reduceAllN :: Expr -> [Expr]
reduceAllN e = if e == stepN e then [e] else e : (reduceAllN $ stepN e)


-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Macro x) = Macro x
stepA (Variable x) = Variable x
stepA (Function x e) = Function x (stepA e)
stepA (Application (Application e1 e2) e3) = Application (stepA (Application e1 e2)) e3
stepA (Application (Function x e1) (Application e2 e3)) = Application (Function x e1) (stepA (Application e2 e3))
stepA (Application (Function x e1) e2) = reduce e1 x e2
stepA (Application e1 e2) = Application e1 (stepA e2)

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expr = if expr == stepA expr then expr else reduceA (stepA expr)

reduceAllA :: Expr -> [Expr]
reduceAllA expr = if expr == stepA expr then [expr] else expr : reduceAllA (stepA expr)

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros env (Variable x) = Variable x
evalMacros env (Function x e) = Function x $ evalMacros env e
evalMacros env (Application e1 e2) = Application (evalMacros env e1) (evalMacros env e2)
evalMacros env (Macro s) = case lookup s env of
                            Nothing -> Macro s
                            Just e -> evalMacros env e

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy code = evalCodeAux strategy [] code 
    where evalCodeAux :: (Expr -> Expr) -> [(String, Expr)] -> [Code] -> [Expr]
          evalCodeAux strategy map [] = []
          evalCodeAux strategy map (x : xs) = case x of
                                                Evaluate e -> strategy (evalMacros map e) : evalCodeAux strategy map xs
                                                Assign x e -> evalCodeAux strategy ((x, e) : map) xs
