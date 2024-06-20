module Lambda where

import Data.List (nub, (\\), find, sort)
import Control.Monad (replicateM)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var e) = [e]
vars (App e1 e2) = nub (vars e1 ++ vars e2)
vars (Abs e l) = nub (e : vars l) 


-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var e) = [e]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs e l) = freeVars l \\ [e]

-- 1.3.
newVar :: [String] -> String
newVar existing = head $ filter (`notElem` existing) generate
  -- Generare combinatii de litere mici, pornind de la siruri de lungime 1, 2, ..
  where generate = concatMap (\n -> replicateM n ['a'..'z']) [1..]

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Abs e1 body) = isNormalForm body
isNormalForm (Macro _) = True

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = subst x e2 e1
  where
    -- Functia subst inlocuieste toate aparitiile variabilei x in e1 cu e2
    subst :: String -> Lambda -> Lambda -> Lambda
    subst x e (Var y) 
      | x == y    = e
      | otherwise = Var y
    subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)
    subst x e (Abs y e1)
      | x == y    = Abs y e1
      | otherwise = Abs z (subst x e (subst y (Var z) e1))
      where z = if y `elem` freeVars e then newVar (vars e ++ vars e1) else y


-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
      | isRegex e1 = App (normalStep e1) e2
      | isRegex e2 = App e1 (normalStep e2)
      | otherwise  = App (normalStep e1) e2
      where
        isRegex (Abs _ _) = True
        isRegex (App _ _) = True
        isRegex _ = False
normalStep (Abs x e) = Abs x (normalStep e)
normalStep expr = expr

-- Verificare daca o expresie lambda este o valoare ~ variabila sau abstractie
isValue :: Lambda -> Bool
isValue (Var _) = True
isValue (Abs _ _) = True
isValue _ = False

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e1) e2) 
  | isValue e2    = reduce x e1 e2
  | otherwise     = App (Abs x e1) (applicativeStep e2)
applicativeStep (App e1 e2) 
  | isValue e1    = App e1 (applicativeStep e2)
  | otherwise     = App (applicativeStep e1) e2 
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep expr = expr


-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify stepFunction expr
    | isNormalForm expr = [expr]
    | otherwise = expr : simplify stepFunction (stepFunction expr)


normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
