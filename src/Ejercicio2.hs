
-- Ejercicio2.hs

data Expr :: * -> * where
    ValV :: Int -> Expr Int
    NotV :: Expr Bool -> Expr Bool
    EqV  :: Expr Int -> Expr Int -> Expr Bool
    AndV :: Expr Bool -> Expr Bool -> Expr Bool
    LtV  :: Expr Int -> Expr Int -> Expr Bool
    OrV  :: Expr Bool -> Expr Bool -> Expr Bool
    
eval :: Expr t -> t
eval (ValV p1)    = p1
eval (NotV p)     = not $ eval p
eval (EqV p1 p2)  = (==) (eval p1) (eval p2)
eval (AndV p1 p2) = (&&) (eval p1) (eval p2)
eval (LtV p1 p2)  = (< ) (eval p1) (eval p2)
eval (OrV p1 p2)  = (||) (eval p1) (eval p2)
