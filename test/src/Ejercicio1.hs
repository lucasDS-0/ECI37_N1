
-- Ejercicio1.hs

class VExpr e where
    valV :: Int    -> e Int
    notV :: e Bool -> e Bool
    eqV  :: e Int  -> e Int  -> e Bool
    andV :: e Bool -> e Bool -> e Bool
    ltV  :: e Int  -> e Int  -> e Bool
    orV  :: e Bool -> e Bool -> e Bool
    
data VEval t = VE t
  deriving Show
  
instance VExpr VEval where
    valV                 = VE
    notV (VE p)          = VE (not p)
    eqV  (VE p1) (VE p2) = VE (p1 == p2)
    andV (VE p1) (VE p2) = VE (p1 && p2)
    ltV  (VE p1) (VE p2) = VE (p1 < p2)
    orV  (VE p1) (VE p2) = VE (p1 || p2)

