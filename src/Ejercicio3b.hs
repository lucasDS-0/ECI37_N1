
-- Ejercicio3b.hs


{--
data UProp  = LOr Term UProp | Term
data Term   = LAnd Factor Term | Factor
data Factor = LNeg UProp | PA UProp | EQ UProp UProp | LT UProp UProp | Num Int
--}

data UProp = Or UProp UProp
           | And UProp UProp 
           | Neg UProp 
           | PA UProp
           | EQ UProp UProp 
           | LT UProp UProp 
           | Num Int
