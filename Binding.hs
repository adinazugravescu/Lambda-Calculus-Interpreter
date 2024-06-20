module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- Face substitutia macro-urilor din context in expresia data
substitute :: Context -> Lambda -> Either String Lambda
substitute ctx (Var x) = Right (Var x)
substitute ctx (Abs var body) = Abs var <$> substitute ctx body
substitute ctx (App e1 e2) = App <$> substitute ctx e1 <*> substitute ctx e2
substitute ctx (Macro name) =
    case lookup name ctx of
        Just expr -> Right expr
        Nothing   -> Left $ "Macro " ++ name ++ " not found"

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx stepFunction expr = do
    expr' <- substitute ctx expr
    return $ simplify stepFunction expr'

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
