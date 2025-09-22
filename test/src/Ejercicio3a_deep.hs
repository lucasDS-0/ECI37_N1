
-- Ejercicio3a_deep.hs

-- Deep Embedding

data Expr :: * -> * where
    ValV :: Int       -> Expr Int
    NotV :: Expr Bool -> Expr Bool
    EqV  :: Expr Int  -> Expr Int  -> Expr Bool
    AndV :: Expr Bool -> Expr Bool -> Expr Bool
    LtV  :: Expr Int  -> Expr Int  -> Expr Bool
    OrV  :: Expr Bool -> Expr Bool -> Expr Bool
    
eval :: Expr t -> t
eval (ValV p)     = p
eval (NotV p)     = not $ eval p
eval (EqV p1 p2)  = (==) (eval p1) (eval p2)
eval (AndV p1 p2) = (&&) (eval p1) (eval p2)
eval (LtV p1 p2)  = (< ) (eval p1) (eval p2)
eval (OrV p1 p2)  = (||) (eval p1) (eval p2)

pprint :: Expr t -> String
pprint (ValV p)     = show p
pprint (NotV p)     = "not" ++ pprint p
pprint (EqV p1 p2)  = pprint p1 ++ " == " ++ pprint p2
pprint (AndV p1 p2) = pprint p1 ++ " && " ++ pprint p2
pprint (LtV p1 p2)  = pprint p1 ++ " < " ++ pprint p2
pprint (OrV p1 p2)  = pprint p1 ++ " || " ++ pprint p2
